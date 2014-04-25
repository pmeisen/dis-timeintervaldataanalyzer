package net.meisen.dissertation.impl.parser.query;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.text.ParseException;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.exceptions.QueryParsingException;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.impl.measures.Average;
import net.meisen.dissertation.impl.measures.Count;
import net.meisen.dissertation.impl.measures.Max;
import net.meisen.dissertation.impl.measures.Min;
import net.meisen.dissertation.impl.parser.query.select.DescriptorValue;
import net.meisen.dissertation.impl.parser.query.select.Interval;
import net.meisen.dissertation.impl.parser.query.select.IntervalType;
import net.meisen.dissertation.impl.parser.query.select.ResultType;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.parser.query.select.evaluator.GroupResult;
import net.meisen.dissertation.impl.parser.query.select.evaluator.SelectResult;
import net.meisen.dissertation.impl.parser.query.select.group.GroupExpression;
import net.meisen.dissertation.impl.parser.query.select.logical.DescriptorLeaf;
import net.meisen.dissertation.impl.parser.query.select.logical.DescriptorLogicTree;
import net.meisen.dissertation.impl.parser.query.select.logical.ITreeElement;
import net.meisen.dissertation.impl.parser.query.select.logical.LogicalOperator;
import net.meisen.dissertation.impl.parser.query.select.logical.LogicalOperatorNode;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.general.genmisc.types.Dates;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the implementation of the {@code QueryFactory}, {@code QueryGenerator}
 * and {@code DescriptorLogicTree}. <br/>
 * <br/>
 * <b>Note:</b><br/>
 * 
 * <table border="0" cellpadding="0" cellspacing="0" width="690">
 * <tr height="20">
 * <td height="20" width=122>Start</td>
 * <td width=122>End</td>
 * <td width=122>PERSON</td>
 * <td width=122>LOCATION</td>
 * <td width=122>SCREAMS</td>
 * <td width=80>#</td>
 * </tr>
 * <tr height="20">
 * <td height="20">03.03.2014 00:00</td>
 * <td>04.03.2014 23:59</td>
 * <td>Tobias</td>
 * <td>Aachen</td>
 * <td>0</td>
 * <td>1</td>
 * </tr>
 * <tr height="20">
 * <td height="20">03.03.2014 00:00</td>
 * <td>03.03.2014 16:19</td>
 * <td>Philipp</td>
 * <td>Mönchengladbach</td>
 * <td>3</td>
 * <td>2</td>
 * </tr>
 * <tr height="20">
 * <td height="20">03.03.2014 16:20</td>
 * <td>03.03.2014 17:21</td>
 * <td>Philipp</td>
 * <td>Undefined</td>
 * <td>0</td>
 * <td>3</td>
 * </tr>
 * <tr height="20">
 * <td height="20">03.03.2014 17:22</td>
 * <td>04.03.2014 23:59</td>
 * <td>Philipp</td>
 * <td>Aachen</td>
 * <td>0</td>
 * <td>4</td>
 * </tr>
 * <tr height="20">
 * <td height="20">03.03.2014 00:00</td>
 * <td>04.03.2014 23:59</td>
 * <td>Debbie</td>
 * <td>Aachen</td>
 * <td>0</td>
 * <td>5</td>
 * </tr>
 * <tr height="20">
 * <td height="20">03.03.2014 00:00</td>
 * <td>04.03.2014 23:59</td>
 * <td>Edison</td>
 * <td>Aachen</td>
 * <td>12</td>
 * <td>6</td>
 * </tr>
 * </table>
 * 
 * @author pmeisen
 * 
 * @see QueryGenerator
 * @see QueryFactory
 * @see DescriptorLogicTree
 * 
 */
public class TestQueryFactory extends LoaderBasedTest {

	@Autowired
	@Qualifier(DefaultValues.QUERYFACTORY_ID)
	private QueryFactory factory;

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
		final SelectQuery query = q("SELECT TIMESERIES FROM MODELID");

		assertEquals("MODELID", query.getModelId());
		assertEquals(ResultType.TIMESERIES, query.getResultType());
		assertNull(query.getInterval());
	}

	/**
	 * Tests the query which selects all records.
	 */
	@Test
	public void testAllRecordsSelectQuery() {
		final SelectQuery query = q("SELECT RECORDS FROM MODELID");

		assertEquals("MODELID", query.getModelId());
		assertEquals(ResultType.RECORDS, query.getResultType());
		assertNull(query.getInterval());
	}

	/**
	 * Tests the query which selects all the timeseries within a specified time
	 * window.
	 */
	@Test
	public void testSelectQueryWithIntegerTimeWindow() {
		final SelectQuery query = q("SELECT TIMESERIES FROM MODELID IN (500 ,600)");

		assertEquals("MODELID", query.getModelId());
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
		final SelectQuery query = q("SELECT TIMESERIES FROM ModelId IN (15.06.2014 , 15.06.2015 20:10:11]");

		assertEquals("ModelId", query.getModelId());
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
		final SelectQuery query = q("select timeSeries from model in [03.03.2014,05.03.2014)");

		assertEquals("model", query.getModelId());

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
		final SelectQuery query = q("select timeSeries from model in [03.03.2014,05.03.2014) filter by singleEqual='singleEqualValue'");
		final DescriptorLogicTree tree = query.getFilter();

		final List<ITreeElement> order = tree.getEvaluationOrder();
		assertEquals(1, order.size());
		assertTrue(order.get(0) instanceof DescriptorLeaf);

		final DescriptorLeaf leaf = (DescriptorLeaf) order.get(0);
		assertEquals("singleEqual", leaf.get().getId());
		assertEquals("singleEqualValue", leaf.get().getRawValue());
	}

	/**
	 * Tests the created {@code DescriptorLogicTree} if a single filter is
	 * defined using unneeded brackets.
	 */
	@Test
	public void testParsingOfSingleValueWithBracketsFilter() {
		final SelectQuery query = q("select timeSeries from model in [03.03.2014,05.03.2014) filter by (((bracketsSingleEqual='bracketsSingleEqualValue')))");
		final DescriptorLogicTree tree = query.getFilter();

		final List<ITreeElement> order = tree.getEvaluationOrder();
		assertEquals(1, order.size());
		assertTrue(order.get(0) instanceof DescriptorLeaf);

		final DescriptorLeaf leaf = (DescriptorLeaf) order.get(0);
		assertEquals("bracketsSingleEqual", leaf.get().getId());
		assertEquals("bracketsSingleEqualValue", leaf.get().getRawValue());
	}

	/**
	 * Tests the parsing of a {@code SelectQuery} without any measure.
	 */
	@Test
	public void testNoMeasures() {
		final SelectQuery query = q("select timeseries from testPersonModel");
		assertEquals(0, query.getMeasures().size());
	}

	/**
	 * Tests the parsing of a {@code SelectQuery} with a single simple measure.
	 * Simple means that just a function with a descriptor is applied.
	 */
	@Test
	public void testSingleSimpleMeasure() {
		final SelectQuery query = q("select timeseries of count(PERSON) from testPersonModel");
		assertEquals(1, query.getMeasures().size());
		assertTrue(query.getMeasures().get(0).getFunction() instanceof Count);
		assertEquals("PERSON", query.getMeasures().get(0)
				.getDescriptorModelId());
	}

	/**
	 * Tests the parsing of a {@code SelectQuery} with multiple measures.
	 */
	@Test
	public void testMultipleSimpleMeasure() {
		final SelectQuery query = q("select timeseries of average(PERSON), min(PERSON), max(LOCATION) from testPersonModel");

		assertEquals(3, query.getMeasures().size());

		assertTrue(query.getMeasures().get(0).getFunction() instanceof Average);
		assertEquals("PERSON", query.getMeasures().get(0)
				.getDescriptorModelId());

		assertTrue(query.getMeasures().get(1).getFunction() instanceof Min);
		assertEquals("PERSON", query.getMeasures().get(1)
				.getDescriptorModelId());

		assertTrue(query.getMeasures().get(2).getFunction() instanceof Max);
		assertEquals("LOCATION", query.getMeasures().get(2)
				.getDescriptorModelId());
	}

	@Test
	public void testComplexMeasure() {
		SelectQuery query;

		query = q("select timeseries of max(LOCATION + PERSON) * (average(PERSON) + min(PERSON)) from testPersonModel");
		System.out.println(query.getMeasures());

	}

	/**
	 * Tests the retrieval of a simple group.
	 */
	@Test
	public void testParsingOfNoGroupBy() {
		final SelectQuery query = q("select timeSeries from model in [03.03.2014,05.03.2014)");
		final GroupExpression group = query.getGroup();

		final Set<String> descriptors = group.getDescriptors();
		assertEquals(0, descriptors.size());
	}

	/**
	 * Tests the retrieval of a simple group.
	 */
	@Test
	public void testParsingOfSimpleGroupBy() {
		final SelectQuery query = q("select timeSeries from model in [03.03.2014,05.03.2014) group by first, second, third");
		final GroupExpression group = query.getGroup();

		final Set<String> descriptors = group.getDescriptors();
		assertEquals(3, descriptors.size());
		final Iterator<String> it = descriptors.iterator();
		assertEquals("first", it.next());
		assertEquals("second", it.next());
		assertEquals("third", it.next());
	}

	/**
	 * Tests the retrieval of a group with defined exclusions.
	 */
	@Test
	public void testParsingOfGroupByWithExclusion() {
		final SelectQuery query = q("select timeSeries from model in [03.03.2014,05.03.2014) group by first, second, third ignore {('\\\\hallo\\\\','pleasematch*','formula is a \\* b')}");
		final GroupExpression group = query.getGroup();

		final Set<String> descriptors = group.getDescriptors();
		assertEquals(3, descriptors.size());
		final Iterator<String> itDesc = descriptors.iterator();
		assertEquals("first", itDesc.next());
		assertEquals("second", itDesc.next());
		assertEquals("third", itDesc.next());

		assertEquals(1, group.getExclusions().size());
		final List<DescriptorValue> excls = group.getExclusions().iterator()
				.next().getValues();
		assertEquals(3, excls.size());
		assertEquals("\\hallo\\", excls.get(0).getValue());
		assertEquals("pleasematch.*", excls.get(1).getValue());
		assertEquals("formula is a * b", excls.get(2).getValue());
	}

	/**
	 * Tests the logical linkage using {@code and} and {@code or}
	 */
	@Test
	public void testLogicalSimpleLinkage() {
		final SelectQuery query = q("select timeSeries from model in [03.03.2014,05.03.2014) filter by first='firstValue' AND second='secondValue' OR third='thirdValue'");
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
		assertEquals("firstValue", first.get().getRawValue());
		assertEquals("second", second.get().getId());
		assertEquals("secondValue", second.get().getRawValue());
		assertEquals("third", third.get().getId());
		assertEquals("thirdValue", third.get().getRawValue());

		assertEquals(LogicalOperator.AND, and.get());
		assertEquals(first, and.getChild(0));
		assertEquals(second, and.getChild(1));

		assertEquals(LogicalOperator.OR, or.get());
		assertEquals(and, or.getChild(0));
		assertEquals(third, or.getChild(1));
	}

	/**
	 * Tests the simple usage of {@code NOT}.
	 */
	@Test
	public void testLogicalSimpleNot() {
		final SelectQuery query = q("select timeseries from model in [03.03.2014,05.03.2014) filter by NOT HALLO='500' AND !HELLO='LALA'");
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
		assertEquals("500", hallo.get().getRawValue());
		assertEquals("HELLO", hello.get().getId());
		assertEquals("LALA", hello.get().getRawValue());

		assertEquals(LogicalOperator.NOT, notHallo.get());
		assertEquals(hallo, notHallo.getChild(0));

		assertEquals(LogicalOperator.NOT, notHello.get());
		assertEquals(hello, notHello.getChild(0));

		assertEquals(LogicalOperator.AND, and.get());
		assertEquals(notHallo, and.getChild(0));
		assertEquals(notHello, and.getChild(1));
	}

	/**
	 * Tests the parsing of a more complex logic using {@code NOT}.
	 */
	@Test
	public void testLogicalComplexNot() {
		final SelectQuery query = q("select timeseries from model in [03.03.2014,05.03.2014) filter by (!(CAKE='APPLE' OR NOT (CAKE='CHERRY')) AND !GREETINGS='HI')");
		final DescriptorLogicTree tree = query.getFilter();

		final List<ITreeElement> order = tree.getEvaluationOrder();

		// check the amount of commands
		assertEquals(8, order.size());

		// check the commands
		// @formatter:off
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

	/**
	 * Tests the usage of nested (i.e. brackets) and combined filters.
	 */
	@Test
	public void testNestedAndCombinedFilters() {
		final SelectQuery query = q("select timeseries from testModel in [03.03.2014,05.03.2014) filter by (LOCATION='Aachen' AND PERSON='Tobias' AND SCREAMS='0') AND (SCREAMS='12' OR PERSON='Debbie' OR LOCATION='Undefined')");
		final DescriptorLogicTree tree = query.getFilter();

		final List<ITreeElement> order = tree.getEvaluationOrder();

		// check the amount of commands
		assertEquals(8, order.size());

		// check the commands
		// @formatter:off
		assertEquals("LOCATION = Aachen", order.get(0).toString());
		assertEquals("PERSON = Tobias", order.get(1).toString());
		assertEquals("SCREAMS = 0", order.get(2).toString());
		assertEquals("SCREAMS = 12", order.get(3).toString());
		assertEquals("PERSON = Debbie", order.get(4).toString());
		assertEquals("LOCATION = Undefined", order.get(5).toString());
		assertEquals("OR(SCREAMS = 12, PERSON = Debbie, LOCATION = Undefined)", order.get(6).toString());
		assertEquals("AND(LOCATION = Aachen, PERSON = Tobias, SCREAMS = 0, OR(SCREAMS = 12, PERSON = Debbie, LOCATION = Undefined))", order.get(7).toString());
		// @formatter:on
	}

	/**
	 * Tests the usage of reserved words as identifiers.
	 */
	@Test
	public void testUsageOfReservedWords() {
		final SelectQuery query = q("select timeseries from \"timeseries\" in [03.03.2014,05.03.2014) filter by \"FROM\"='1'");
		assertEquals("timeseries", query.getModelId());

		final DescriptorLogicTree tree = query.getFilter();
		final List<ITreeElement> order = tree.getEvaluationOrder();
		assertEquals(1, order.size());

		final Object o = order.iterator().next();
		assertTrue(o instanceof DescriptorLeaf);
		assertEquals("FROM", ((DescriptorLeaf) o).get().getId());
	}

	/**
	 * Tests the logical optimization for a simple query.
	 */
	@Test
	public void testLogicalSimpleOptimization() {
		final SelectQuery query = q("select timeseries from model in [03.03.2014,05.03.2014) filter by first='1' AND second='2' AND third='3'");
		final DescriptorLogicTree tree = query.getFilter();

		final List<ITreeElement> order = tree.getEvaluationOrder();

		assertEquals(4, order.size());
	}

	/**
	 * Tests the optimization for a compley query.
	 */
	@Test
	public void testLogicalComplexOptimization() {
		final SelectQuery query = q("select timeseries from model in [03.03.2014,05.03.2014) filter by (first='1' OR second='2') AND (third='3' OR firth='4') AND fifth='5' AND (sixth='6' OR (seventh='7' AND eight='8'))");
		final DescriptorLogicTree tree = query.getFilter();

		final List<ITreeElement> order = tree.getEvaluationOrder();

		// check the amount of commands
		assertEquals(13, order.size());

		// check the commands
		// @formatter:off
		assertEquals("first = 1", order.get(0).toString());
		assertEquals("second = 2", order.get(1).toString());
		assertEquals("OR(first = 1, second = 2)", order.get(2).toString());
		assertEquals("third = 3", order.get(3).toString());
		assertEquals("firth = 4", order.get(4).toString());
		assertEquals("OR(third = 3, firth = 4)", order.get(5).toString());
		assertEquals("fifth = 5", order.get(6).toString());
		assertEquals("sixth = 6", order.get(7).toString());
		assertEquals("seventh = 7", order.get(8).toString());
		assertEquals("eight = 8", order.get(9).toString());
		assertEquals("AND(seventh = 7, eight = 8)", order.get(10).toString());
		assertEquals("OR(sixth = 6, AND(seventh = 7, eight = 8))", order.get(11).toString());
		assertEquals("AND(OR(first = 1, second = 2), OR(third = 3, firth = 4), fifth = 5, OR(sixth = 6, AND(seventh = 7, eight = 8)))", order.get(12).toString());
		// @formatter:on
	}

	/**
	 * Tests the exception to be thrown when concatenating select-statements.
	 */
	@Test
	public void testConcatenatedSelectsException() {
		thrown.expect(QueryParsingException.class);
		thrown.expectMessage("syntax error was found at 1:56");

		q("select timeseries from model in [03.03.2014,05.03.2014) select timeseries from model in [03.03.2014,05.03.2014)");
	}

	/**
	 * Tests the exception to be thrown when concatenating select-statements.
	 */
	@Test
	public void testNestedSelectsException() {
		thrown.expect(QueryParsingException.class);
		thrown.expectMessage("syntax error was found at 1:23");

		q("select timeseries from (select timeseries from model in [03.03.2014,05.03.2014)) in [03.03.2014,05.03.2014)");
	}

	/**
	 * Tests no filter.
	 */
	@Test
	public void testExecutionWithNoFilter() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014)";

		// load the model
		m(xml);

		// fire the query
		final SelectResult res = (SelectResult) factory.evaluateQuery(q(query),
				loader);

		// check the result's filter
		assertNull(res.getFilterResult());
	}

	/**
	 * Tests a simple filter.
	 */
	@Test
	public void testExecutionWithSingleFilter() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014) filter by SCREAMS='3'";

		// load the model
		m(xml);

		// fire the query
		final SelectResult res = (SelectResult) factory.evaluateQuery(q(query),
				loader);

		// check the result's filter
		final int[] filterRes = res.getFilterResult().getBitmap().getIds();
		assertEquals(1, filterRes.length);
		assertFalse(Arrays.binarySearch(filterRes, 0) > -1);
		assertTrue(Arrays.binarySearch(filterRes, 1) > -1);
		assertFalse(Arrays.binarySearch(filterRes, 2) > -1);
		assertFalse(Arrays.binarySearch(filterRes, 3) > -1);
		assertFalse(Arrays.binarySearch(filterRes, 4) > -1);
		assertFalse(Arrays.binarySearch(filterRes, 5) > -1);
	}

	/**
	 * Tests a more complex filter.
	 */
	@Test
	public void testExecutionWithComplexFilter() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014) filter by (SCREAMS='3' OR SCREAMS='0') AND PERSON='Philipp'";

		// load the model
		m(xml);

		// fire the query
		final SelectResult res = (SelectResult) factory.evaluateQuery(q(query),
				loader);

		// check the result's filter
		final int[] filterRes = res.getFilterResult().getBitmap().getIds();
		assertEquals(3, filterRes.length);
		assertFalse(Arrays.binarySearch(filterRes, 0) > -1);
		assertTrue(Arrays.binarySearch(filterRes, 1) > -1);
		assertTrue(Arrays.binarySearch(filterRes, 2) > -1);
		assertTrue(Arrays.binarySearch(filterRes, 3) > -1);
		assertFalse(Arrays.binarySearch(filterRes, 4) > -1);
		assertFalse(Arrays.binarySearch(filterRes, 5) > -1);
	}

	/**
	 * Tests the usage of a or-filter, filtering with a value not defined within
	 * the descriptor.
	 */
	@Test
	public void testExecutionWithUnusedValueFilterOr() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014) filter by SCREAMS='Undefined' OR PERSON='Philipp'";

		// load the model
		m(xml);

		// fire the query
		final SelectResult res = (SelectResult) factory.evaluateQuery(q(query),
				loader);

		// check the result's filter
		final int[] filterRes = res.getFilterResult().getBitmap().getIds();
		assertEquals(3, filterRes.length);
		assertFalse(Arrays.binarySearch(filterRes, 0) > -1);
		assertTrue(Arrays.binarySearch(filterRes, 1) > -1);
		assertTrue(Arrays.binarySearch(filterRes, 2) > -1);
		assertTrue(Arrays.binarySearch(filterRes, 3) > -1);
		assertFalse(Arrays.binarySearch(filterRes, 4) > -1);
		assertFalse(Arrays.binarySearch(filterRes, 5) > -1);
	}

	/**
	 * Tests the usage of a and-filter, filtering with a value not defined
	 * within the descriptor.
	 */
	@Test
	public void testExecutionWithUnusedValueFilterAnd() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014) filter by SCREAMS='Undefined' AND PERSON='Philipp'";

		// load the model
		m(xml);

		// fire the query
		final SelectResult res = (SelectResult) factory.evaluateQuery(q(query),
				loader);

		// check the result's filter
		final int[] filterRes = res.getFilterResult().getBitmap().getIds();
		assertEquals(0, filterRes.length);
		assertFalse(Arrays.binarySearch(filterRes, 0) > -1);
		assertFalse(Arrays.binarySearch(filterRes, 1) > -1);
		assertFalse(Arrays.binarySearch(filterRes, 2) > -1);
		assertFalse(Arrays.binarySearch(filterRes, 3) > -1);
		assertFalse(Arrays.binarySearch(filterRes, 4) > -1);
		assertFalse(Arrays.binarySearch(filterRes, 5) > -1);
	}

	/**
	 * Tests the usage of a nested, i.e. using brakets, filtering.
	 */
	@Test
	public void testNestedFiltering() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014) filter by (LOCATION='Aachen' AND PERSON='Tobias' AND SCREAMS='0') OR (SCREAMS='12' OR (PERSON='Debbie' AND LOCATION='Aachen') OR LOCATION='Undefined')";

		// load the model
		m(xml);

		// fire the query
		final SelectResult res = (SelectResult) factory.evaluateQuery(q(query),
				loader);

		// check the result's filter
		final int[] filterRes = res.getFilterResult().getBitmap().getIds();
		assertEquals(4, filterRes.length);
		assertTrue(Arrays.binarySearch(filterRes, 0) > -1);
		assertFalse(Arrays.binarySearch(filterRes, 1) > -1);
		assertTrue(Arrays.binarySearch(filterRes, 2) > -1);
		assertFalse(Arrays.binarySearch(filterRes, 3) > -1);
		assertTrue(Arrays.binarySearch(filterRes, 4) > -1);
		assertTrue(Arrays.binarySearch(filterRes, 5) > -1);
	}

	/**
	 * Tests the implementation of NOT within a filter.
	 */
	@Test
	public void testSimpleNotSelection() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014) filter by NOT((LOCATION='Aachen' AND PERSON='Tobias' AND SCREAMS='0') OR (SCREAMS='12' OR (PERSON='Debbie' AND LOCATION='Aachen') OR LOCATION='Undefined'))";

		// load the model
		m(xml);

		// fire the query
		final SelectResult res = (SelectResult) factory.evaluateQuery(q(query),
				loader);

		// check the result's filter
		final int[] filterRes = res.getFilterResult().getBitmap().getIds();
		assertEquals(2, filterRes.length);
		assertFalse(Arrays.binarySearch(filterRes, 0) > -1);
		assertTrue(Arrays.binarySearch(filterRes, 1) > -1);
		assertFalse(Arrays.binarySearch(filterRes, 2) > -1);
		assertTrue(Arrays.binarySearch(filterRes, 3) > -1);
		assertFalse(Arrays.binarySearch(filterRes, 4) > -1);
		assertFalse(Arrays.binarySearch(filterRes, 5) > -1);
	}

	/**
	 * Tests the usage of several NOT operations.
	 */
	@Test
	public void testSeveralNotSelection() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014) filter by NOT(LOCATION='Aachen') AND NOT(PERSON='Philipp')";

		// load the model
		m(xml);

		// fire the query
		final SelectResult res = (SelectResult) factory.evaluateQuery(q(query),
				loader);

		// check the result's filter
		final int[] filterRes = res.getFilterResult().getBitmap().getIds();
		assertEquals(0, filterRes.length);
	}

	/**
	 * Tests the selection using a {@code NULL} value.
	 */
	@Test
	public void testNullSelection() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testCleanedModel.xml";
		final String query = "select timeseries from testCleanedModel in [01.01.2014,31.12.2014] filter by PRIORITY=NULL";

		// load the model
		m(xml);

		// fire the query
		final SelectResult res = (SelectResult) factory.evaluateQuery(q(query),
				loader);

		// check the result's filter
		final int[] filterRes = res.getFilterResult().getBitmap().getIds();
		assertFalse(Arrays.binarySearch(filterRes, 0) > -1);
		assertFalse(Arrays.binarySearch(filterRes, 1) > -1);
		assertFalse(Arrays.binarySearch(filterRes, 2) > -1);
		assertTrue(Arrays.binarySearch(filterRes, 3) > -1);
	}

	/**
	 * Tests the {@code SelectResult} when no {@code group by} is defined.
	 */
	@Test
	public void testNoGroupBy() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014)";

		// load the model
		m(xml);

		// fire the query
		final SelectResult res = (SelectResult) factory.evaluateQuery(q(query),
				loader);

		// check the result's filter
		assertNull(res.getGroupResult());
	}

	/**
	 * Tests the {@code SelectResult} when a single {@code group by} is defined.
	 */
	@Test
	public void testSimpleGroupBy() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014) group by PERSON";

		// load the model
		m(xml);

		// fire the query
		final SelectResult res = (SelectResult) factory.evaluateQuery(q(query),
				loader);

		// check the result's filter
		final GroupResult gRes = res.getGroupResult();
		assertNotNull(gRes);
		assertEquals(4, gRes.size());
		assertNotNull(gRes.getEntry("Philipp"));
		assertNotNull(gRes.getEntry("Debbie"));
		assertNotNull(gRes.getEntry("Tobias"));
		assertNotNull(gRes.getEntry("Edison"));
	}

	/**
	 * Tests the {@code SelectResult} of a statement using two descriptors.
	 */
	@Test
	public void testMultipleGroupBy() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014) group by LOCATION, PERSON";

		// load the model
		m(xml);

		// fire the query
		final SelectResult res = (SelectResult) factory.evaluateQuery(q(query),
				loader);

		// check the result's filter
		final GroupResult gRes = res.getGroupResult();
		assertNotNull(gRes);
		assertEquals(12, gRes.size());
		assertNotNull(gRes.getEntry("Aachen", "Philipp"));
		assertNotNull(gRes.getEntry("Mönchengladbach", "Philipp"));
		assertNotNull(gRes.getEntry("Undefined", "Philipp"));
		assertNotNull(gRes.getEntry("Aachen", "Debbie"));
		assertNotNull(gRes.getEntry("Mönchengladbach", "Debbie"));
		assertNotNull(gRes.getEntry("Undefined", "Debbie"));
		assertNotNull(gRes.getEntry("Aachen", "Tobias"));
		assertNotNull(gRes.getEntry("Mönchengladbach", "Tobias"));
		assertNotNull(gRes.getEntry("Undefined", "Tobias"));
		assertNotNull(gRes.getEntry("Aachen", "Edison"));
		assertNotNull(gRes.getEntry("Mönchengladbach", "Edison"));
		assertNotNull(gRes.getEntry("Undefined", "Edison"));
	}

	/**
	 * Tests a simple {@code group by} expression with some ignored values.
	 */
	@Test
	public void testGroupByWithExclusion() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014) group by PERSON ignore {('Philipp'), ('Tobias')}";

		// load the model
		m(xml);

		// fire the query
		final SelectResult res = (SelectResult) factory.evaluateQuery(q(query),
				loader);

		// check the result's filter
		final GroupResult gRes = res.getGroupResult();
		assertNotNull(gRes);
		assertEquals(2, gRes.size());
		assertNotNull(gRes.getEntry("Debbie"));
		assertNotNull(gRes.getEntry("Edison"));
	}

	@Test
	public void testAllTimeSelection() {

		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select timeseries from testPersonModel";

		// load the model
		m(xml);

		// fire the query
		final SelectResult res = (SelectResult) factory.evaluateQuery(q(query),
				loader);

		// check the result's filter
		System.out.println(res.getTimeSeriesResult());
	}

	@Test
	public void testTimeSelection() {

		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014)";

		// load the model
		m(xml);

		// fire the query
		final SelectResult res = (SelectResult) factory.evaluateQuery(q(query),
				loader);

		// check the result's filter
		System.out.println(res.getTimeSeriesResult());
	}

	/**
	 * Tests the exception expected if an invalid model is used.
	 */
	@Test
	public void testInvalidModel() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select timeseries from testInvalidModel in [03.03.2014,05.03.2014) filter by INVALID='3' OR SCREAMS='0' AND PERSON='Philipp'";

		thrown.expect(QueryEvaluationException.class);
		thrown.expectMessage("Unable to find a model with identifier 'testInvalidModel'");

		// load the model
		m(xml);

		// fire the query
		factory.evaluateQuery(q(query), loader);
	}

	/**
	 * Tests the exception expected if an invalid descriptor is used.
	 */
	@Test
	public void testInvalidDescriptor() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014) filter by INVALID='3' OR SCREAMS='0' AND PERSON='Philipp'";

		thrown.expect(QueryEvaluationException.class);
		thrown.expectMessage("Descriptors with identifiers 'INVALID' cannot be found");

		// load the model
		m(xml);

		// fire the query
		factory.evaluateQuery(q(query), loader);
	}
}
