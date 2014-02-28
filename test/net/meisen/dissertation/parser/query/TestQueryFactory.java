package net.meisen.dissertation.parser.query;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.text.ParseException;
import java.util.Date;
import java.util.List;

import net.meisen.dissertation.parser.query.select.Interval;
import net.meisen.dissertation.parser.query.select.IntervalType;
import net.meisen.dissertation.parser.query.select.ResultType;
import net.meisen.dissertation.parser.query.select.SelectQuery;
import net.meisen.dissertation.parser.query.select.logical.DescriptorLeaf;
import net.meisen.dissertation.parser.query.select.logical.DescriptorLogicTree;
import net.meisen.dissertation.parser.query.select.logical.ITreeElement;
import net.meisen.dissertation.parser.query.select.logical.LogicalOperator;
import net.meisen.dissertation.parser.query.select.logical.LogicalOperatorNode;
import net.meisen.general.genmisc.types.Dates;

import org.junit.Test;

/**
 * Tests the implementation of the {@code QueryFactory}, {@code QueryGenerator}
 * and {@code DescriptorLogicTree}.
 * 
 * @author pmeisen
 * 
 * @see QueryGenerator
 * @see QueryFactory
 * @see DescriptorLogicTree
 * 
 */
public class TestQueryFactory {

	private final QueryFactory factory = new QueryFactory();

	/**
	 * Get the factory's result for the {@code query}.
	 * 
	 * @param query
	 *            the query to be created
	 * @return the created {@code Query}
	 */
	@SuppressWarnings("unchecked")
	protected <T extends IQuery> T q(final String query) {
		return (T) factory.parseQuery(query);
	}

	/**
	 * Tests the query which selects all timeseries.
	 */
	@Test
	public void testAllTimeSeriesSelectQuery() {
		final SelectQuery query = q("SELECT TIMESERIES");

		assertEquals(ResultType.TIMESERIES, query.getResultType());
		assertNull(query.getInterval());
	}

	/**
	 * Tests the query which selects all records.
	 */
	@Test
	public void testAllRecordsSelectQuery() {
		final SelectQuery query = q("SELECT RECORDS");

		assertEquals(ResultType.RECORDS, query.getResultType());
		assertNull(query.getInterval());
	}

	/**
	 * Tests the query which selects all the timeseries within a specified time
	 * window.
	 */
	@Test
	public void testSelectQueryWithIntegerTimeWindow() {
		final SelectQuery query = q("SELECT TIMESERIES IN (500 ,600)");

		assertEquals(ResultType.TIMESERIES, query.getResultType());

		final Interval<?> interval = query.getInterval();
		assertNotNull(interval);
		assertTrue(interval.getStart() instanceof Long);

		assertEquals(new Long(500), interval.getStart());
		assertEquals(new Long(600), interval.getEnd());
		assertEquals(IntervalType.EXCLUDE, interval.getOpenType());
		assertEquals(IntervalType.EXCLUDE, interval.getCloseType());
	}

	/**
	 * Tests the query which selects all the timeseries within a specified time
	 * window.
	 * 
	 * @throws ParseException
	 *             if an expected date cannot be parsed
	 */
	@Test
	public void testSelectQueryWithDateTimeWindow() throws ParseException {
		final SelectQuery query = q("SELECT TIMESERIES IN (15.06.2014 , 15.06.2015 20:10:11]");

		assertEquals(ResultType.TIMESERIES, query.getResultType());

		final Interval<?> interval = query.getInterval();
		assertNotNull(interval);
		assertTrue(interval.getStart() instanceof Date);

		assertEquals(Dates.parseDate("15.06.2014", "dd.MM.yyyy"),
				interval.getStart());
		assertEquals(
				Dates.parseDate("15.06.2015 20:10:11", "dd.MM.yyyy HH:mm:ss"),
				interval.getEnd());
		assertEquals(IntervalType.EXCLUDE, interval.getOpenType());
		assertEquals(IntervalType.INCLUDE, interval.getCloseType());
	}

	/**
	 * Tests the created {@code DescriptorLogicTree} when no filter is defined.
	 */
	public void testParsingOfNoFilter() {
		final SelectQuery query = q("select timeSeries in [15.06.2014,20.01.2015]");
		final DescriptorLogicTree tree = query.getFilter();

		final List<ITreeElement> order = tree.getEvaluationOrder();
		assertEquals(0, order.size());
	}

	/**
	 * Tests the created {@code DescriptorLogicTree} ig just a single filter is
	 * defined.
	 */
	@Test
	public void testParsingOfSingleValueFilter() {
		final SelectQuery query = q("select timeSeries in [15.06.2014,20.01.2015] filter by singleEqual='singleEqualValue'");
		final DescriptorLogicTree tree = query.getFilter();

		final List<ITreeElement> order = tree.getEvaluationOrder();
		assertEquals(1, order.size());
		assertTrue(order.get(0) instanceof DescriptorLeaf);

		final DescriptorLeaf leaf = (DescriptorLeaf) order.get(0);
		assertEquals("singleEqual", leaf.get().getId());
		assertEquals("singleEqualValue", leaf.get().getValue());
	}

	/**
	 * Tests the created {@code DescriptorLogicTree} if a single filter is
	 * defined using unneeded brackets.
	 */
	@Test
	public void testParsingOfSingleValueWithBracketsFilter() {
		final SelectQuery query = q("select timeSeries in [15.06.2014,20.01.2015] filter by (((bracketsSingleEqual='bracketsSingleEqualValue')))");
		final DescriptorLogicTree tree = query.getFilter();

		final List<ITreeElement> order = tree.getEvaluationOrder();
		assertEquals(1, order.size());
		assertTrue(order.get(0) instanceof DescriptorLeaf);

		final DescriptorLeaf leaf = (DescriptorLeaf) order.get(0);
		assertEquals("bracketsSingleEqual", leaf.get().getId());
		assertEquals("bracketsSingleEqualValue", leaf.get().getValue());
	}

	/**
	 * Tests the logical linkage using {@code and} and {@code or}
	 */
	@Test
	public void testLogicalSimpleLinkage() {
		final SelectQuery query = q("select timeSeries in [15.06.2014,20.01.2015] filter by first='firstValue' AND second='secondValue' OR third='thirdValue'");
		final DescriptorLogicTree tree = query.getFilter();

		final List<ITreeElement> order = tree.getEvaluationOrder();

		// check the amount of commands
		assertEquals(5, order.size());

		// check the types of commands
		assertTrue(order.get(0) instanceof DescriptorLeaf);
		assertTrue(order.get(1) instanceof DescriptorLeaf);
		assertTrue(order.get(2) instanceof LogicalOperatorNode);
		assertTrue(order.get(3) instanceof DescriptorLeaf);
		assertTrue(order.get(4) instanceof LogicalOperatorNode);

		// get the tree's elements
		final DescriptorLeaf first = (DescriptorLeaf) order.get(0);
		final DescriptorLeaf second = (DescriptorLeaf) order.get(1);
		final LogicalOperatorNode and = (LogicalOperatorNode) order.get(2);
		final DescriptorLeaf third = (DescriptorLeaf) order.get(3);
		final LogicalOperatorNode or = (LogicalOperatorNode) order.get(4);

		// check the values
		assertEquals("first", first.get().getId());
		assertEquals("firstValue", first.get().getValue());
		assertEquals("second", second.get().getId());
		assertEquals("secondValue", second.get().getValue());
		assertEquals("third", third.get().getId());
		assertEquals("thirdValue", third.get().getValue());

		assertEquals(LogicalOperator.AND, and.get());
		assertEquals(first, and.getChild(0));
		assertEquals(second, and.getChild(1));

		assertEquals(LogicalOperator.OR, or.get());
		assertEquals(and, or.getChild(0));
		assertEquals(third, or.getChild(1));
	}

	/**
	 * Tests the simple usage of {@code not}.
	 */
	@Test
	public void testLogicalSimpleNot() {
		final SelectQuery query = q("select timeseries in [15.06.2014,20.01.2015] filter by NOT HALLO='500' AND !HELLO='LALA'");
		final DescriptorLogicTree tree = query.getFilter();

		final List<ITreeElement> order = tree.getEvaluationOrder();

		// check the amount of commands
		assertEquals(5, order.size());

		// check the types of commands
		assertTrue(order.get(0) instanceof DescriptorLeaf);
		assertTrue(order.get(1) instanceof LogicalOperatorNode);
		assertTrue(order.get(2) instanceof DescriptorLeaf);
		assertTrue(order.get(3) instanceof LogicalOperatorNode);
		assertTrue(order.get(4) instanceof LogicalOperatorNode);

		// get the tree's elements
		final DescriptorLeaf hallo = (DescriptorLeaf) order.get(0);
		final LogicalOperatorNode notHallo = (LogicalOperatorNode) order.get(1);
		final DescriptorLeaf hello = (DescriptorLeaf) order.get(2);
		final LogicalOperatorNode notHello = (LogicalOperatorNode) order.get(3);
		final LogicalOperatorNode and = (LogicalOperatorNode) order.get(4);

		// check the values
		assertEquals("HALLO", hallo.get().getId());
		assertEquals("500", hallo.get().getValue());
		assertEquals("HELLO", hello.get().getId());
		assertEquals("LALA", hello.get().getValue());

		assertEquals(LogicalOperator.NOT, notHallo.get());
		assertEquals(hallo, notHallo.getChild(0));

		assertEquals(LogicalOperator.NOT, notHello.get());
		assertEquals(hello, notHello.getChild(0));

		assertEquals(LogicalOperator.AND, and.get());
		assertEquals(notHallo, and.getChild(0));
		assertEquals(notHello, and.getChild(1));
	}

	/**
	 * Tests the parsing of a more complex logic using {@code not}.
	 */
	@Test
	public void testLogicalComplexNot() {
		final SelectQuery query = q("select timeseries in [15.06.2014,20.01.2015] filter by (!(CAKE='APPLE' OR NOT (CAKE='CHERRY')) AND !GREETINGS='HI')");
		final DescriptorLogicTree tree = query.getFilter();

		final List<ITreeElement> order = tree.getEvaluationOrder();

		// check the amount of commands
		// @formatter:off
		assertEquals(8, order.size());
		assertEquals("CAKE = APPLE", order.get(0).toString());
		assertEquals("CAKE = CHERRY", order.get(1).toString());
		assertEquals("NOT(CAKE = CHERRY)", order.get(2).toString());
		assertEquals("OR(CAKE = APPLE, NOT(CAKE = CHERRY))", order.get(3).toString());
		assertEquals("NOT(OR(CAKE = APPLE, NOT(CAKE = CHERRY)))", order.get(4).toString());
		assertEquals("GREETINGS = HI", order.get(5).toString());
		assertEquals("NOT(GREETINGS = HI)", order.get(6).toString());
		assertEquals("AND(NOT(OR(CAKE = APPLE, NOT(CAKE = CHERRY))), NOT(GREETINGS = HI))", order.get(7).toString());
		// @formatter:on
	}
	
	@Test
	public void testLogicalOptimization() {
		
	}

	@Test
	public void testStringParsing() {

		System.out.println("3");
		factory.parseQuery("select timeseries in [15.06.2014,20.01.2015] filter by (singleEqual='singleEqualValue' AND !(lulu='test' OR lala='5')) AND lulu='test2' OR (lala='250' AND lulu='123')");
	}
}
