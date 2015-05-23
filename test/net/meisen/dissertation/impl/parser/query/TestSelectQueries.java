package net.meisen.dissertation.impl.parser.query;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.text.ParseException;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.GroupEvaluatorException;
import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.exceptions.QueryParsingException;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.impl.measures.Count;
import net.meisen.dissertation.impl.measures.Max;
import net.meisen.dissertation.impl.measures.Mean;
import net.meisen.dissertation.impl.measures.Median;
import net.meisen.dissertation.impl.measures.Min;
import net.meisen.dissertation.impl.measures.Mode;
import net.meisen.dissertation.impl.measures.Sum;
import net.meisen.dissertation.impl.parser.query.select.DescriptorComperator;
import net.meisen.dissertation.impl.parser.query.select.DescriptorValue;
import net.meisen.dissertation.impl.parser.query.select.DimensionComperator;
import net.meisen.dissertation.impl.parser.query.select.IntervalRelation;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.parser.query.select.SelectResult;
import net.meisen.dissertation.impl.parser.query.select.SelectResultRecords;
import net.meisen.dissertation.impl.parser.query.select.SelectResultTimeSeries;
import net.meisen.dissertation.impl.parser.query.select.SelectResultType;
import net.meisen.dissertation.impl.parser.query.select.evaluator.GroupResult;
import net.meisen.dissertation.impl.parser.query.select.evaluator.GroupResultEntry;
import net.meisen.dissertation.impl.parser.query.select.group.GroupExpression;
import net.meisen.dissertation.impl.parser.query.select.logical.DescriptorLeaf;
import net.meisen.dissertation.impl.parser.query.select.logical.DescriptorLogicTree;
import net.meisen.dissertation.impl.parser.query.select.logical.ILogicalTreeElement;
import net.meisen.dissertation.impl.parser.query.select.logical.LogicalOperator;
import net.meisen.dissertation.impl.parser.query.select.logical.LogicalOperatorNode;
import net.meisen.dissertation.impl.parser.query.select.measures.ArithmeticOperator;
import net.meisen.dissertation.impl.parser.query.select.measures.DescriptorMathTree;
import net.meisen.dissertation.impl.parser.query.select.measures.IMathTreeElement;
import net.meisen.dissertation.impl.parser.query.select.measures.MathOperator;
import net.meisen.dissertation.impl.parser.query.select.measures.MathOperatorNode;
import net.meisen.dissertation.impl.time.series.TimeSeries;
import net.meisen.dissertation.impl.time.series.TimeSeriesCollection;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.measures.IDimAggregationFunction;
import net.meisen.dissertation.model.measures.ILowAggregationFunction;
import net.meisen.dissertation.model.measures.IMathAggregationFunction;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.general.genmisc.collections.Collections;
import net.meisen.general.genmisc.types.Dates;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.genmisc.types.Numbers;

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
public class TestSelectQueries extends LoaderBasedTest {
	private final static Class<?> mathDescLeaf = net.meisen.dissertation.impl.parser.query.select.measures.DescriptorLeaf.class;

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
		final SelectQuery query = q("SELECT TIMESERIES from MODELID");

		assertEquals("MODELID", query.getModelId());
		assertEquals(SelectResultType.TIMESERIES, query.getResultType());
		assertFalse(query.isTransposed());
		assertNull(query.getInterval());
	}

	/**
	 * Tests the usage of a transposed timeseries.
	 */
	@Test
	public void testTransposition() {
		final SelectQuery query = q("SELECT TRANSPOSE(TIMESERIES) from MODELID");

		assertEquals("MODELID", query.getModelId());
		assertEquals(SelectResultType.TIMESERIES, query.getResultType());
		assertTrue(query.isTransposed());
		assertNull(query.getInterval());
	}

	/**
	 * Tests the query which selects all records.
	 */
	@Test
	public void testAllRecordsSelectQuery() {
		final SelectQuery query = q("SELECT RECORDS from MODELID");

		assertEquals("MODELID", query.getModelId());
		assertEquals(SelectResultType.RECORDS, query.getResultType());
		assertFalse(query.isTransposed());
		assertNull(query.getInterval());
	}

	/**
	 * Tests the query which uses a recordId filter.
	 */
	@Test
	public void testRecordIdFilter() {
		final SelectQuery query = q("SELECT RECORDS from MODELID WHERE [ID]=5");

		assertEquals("MODELID", query.getModelId());
		assertEquals(SelectResultType.RECORDS, query.getResultType());
		assertFalse(query.isTransposed());
		assertNull(query.getInterval());
		assertArrayEquals(new int[] { 5 }, query.getRecordIdFilter());
	}

	/**
	 * Tests the query which selects the identifiers of all records.
	 */
	@Test
	public void testAllRecordIdsSelectQuery() {
		final SelectQuery query = q("SELECT IDS(RECORDS) from MODELID");

		assertEquals("MODELID", query.getModelId());
		assertEquals(SelectResultType.RECORDS, query.getResultType());
		assertFalse(query.isTransposed());
		assertNull(query.getInterval());
	}

	/**
	 * Tests the query which selects only the count of selected records.
	 */
	@Test
	public void testAllRecordsCountSelectQuery() {
		final SelectQuery query = q("SELECT COUNT(RECORDS) from MODELID");

		assertEquals("MODELID", query.getModelId());
		assertEquals(SelectResultType.RECORDS, query.getResultType());
		assertFalse(query.isTransposed());
		assertNull(query.getInterval());
	}

	/**
	 * Tests the query which defines different interval-relations.
	 */
	@Test
	public void testRecordSelectQueryWithIntervalRelation() {
		SelectQuery query;

		query = q("SELECT RECORDS from modelId");
		assertNull(query.getIntervalRelation());

		query = q("SELECT RECORDS from modelId DURING [5, 9]");
		assertEquals(IntervalRelation.DURING, query.getIntervalRelation());

		query = q("SELECT COUNT(RECORDS) from modelId CONtaining [5, 9] FILTER BY DESC='VAL'");
		assertEquals(IntervalRelation.CONTAINING, query.getIntervalRelation());

		query = q("SELECT IDS(RECORDS) from modelId before [5, 9]");
		assertEquals(IntervalRelation.BEFORE, query.getIntervalRelation());

		query = q("SELECT RECORDS from modelId after [5, 9]");
		assertEquals(IntervalRelation.AFTER, query.getIntervalRelation());

		query = q("SELECT RECORDS from modelId equalto [5, 9]");
		assertEquals(IntervalRelation.EQUALTO, query.getIntervalRelation());

		query = q("SELECT RECORDS from modelId finishingwith [5, 9]");
		assertEquals(IntervalRelation.FINISHINGWITH,
				query.getIntervalRelation());

		query = q("SELECT RECORDS from modelId startingwiTH [5, 9]");
		assertEquals(IntervalRelation.STARTINGWITH, query.getIntervalRelation());

		query = q("SELECT RECORDS from modelId overlapping [5, 9]");
		assertEquals(IntervalRelation.OVERLAPPING, query.getIntervalRelation());

		query = q("SELECT RECORDS from modelId meeting [5, 9]");
		assertEquals(IntervalRelation.MEETING, query.getIntervalRelation());
	}

	/**
	 * Tests the query which selects all the timeseries within a specified time
	 * window.
	 */
	@Test
	public void testSelectQueryWithIntegerTimeWindow() {
		final SelectQuery query = q("SELECT TIMESERIES from modelId IN (500 ,600)");

		assertEquals("modelId", query.getModelId());
		assertEquals(SelectResultType.TIMESERIES, query.getResultType());

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
		final SelectQuery query = q("SELECT TIMESERIES from ModelId IN (15.06.2014 , 15.06.2015 20:10:11]");

		assertEquals("ModelId", query.getModelId());
		assertEquals(SelectResultType.TIMESERIES, query.getResultType());

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
		final SelectQuery query = q("select timeSeries from \"model\" in [03.03.2014,05.03.2014)");

		assertEquals("model", query.getModelId());

		final DescriptorLogicTree tree = query.getFilter();

		final List<ILogicalTreeElement> order = tree.getEvaluationOrder();
		assertEquals(0, order.size());
	}

	/**
	 * Tests the created {@code DescriptorLogicTree} ig just a single filter is
	 * defined.
	 */
	@Test
	public void testParsingOfSingleValueFilter() {
		final SelectQuery query = q("select timeSeries from \"model\" in [03.03.2014,05.03.2014) filter by singleEqual='singleEqualValue'");
		final DescriptorLogicTree tree = query.getFilter();

		final List<ILogicalTreeElement> order = tree.getEvaluationOrder();
		assertEquals(1, order.size());
		assertTrue(order.get(0) instanceof DescriptorLeaf);

		final DescriptorLeaf leaf = (DescriptorLeaf) order.get(0);
		assertEquals("singleEqual", ((DescriptorComperator) leaf.get()).getId());
		assertEquals("singleEqualValue",
				((DescriptorComperator) leaf.get()).getRawValue());
	}

	/**
	 * Tests the created {@code DescriptorLogicTree} if a single filter is
	 * defined using unneeded brackets.
	 */
	@Test
	public void testParsingOfSingleValueWithBracketsFilter() {
		final SelectQuery query = q("select timeSeries from \"model\" in [03.03.2014,05.03.2014) filter by (((bracketsSingleEqual='bracketsSingleEqualValue')))");
		final DescriptorLogicTree tree = query.getFilter();

		final List<ILogicalTreeElement> order = tree.getEvaluationOrder();
		assertEquals(1, order.size());
		assertTrue(order.get(0) instanceof DescriptorLeaf);

		final DescriptorLeaf leaf = (DescriptorLeaf) order.get(0);
		assertEquals("bracketsSingleEqual",
				((DescriptorComperator) leaf.get()).getId());
		assertEquals("bracketsSingleEqualValue",
				((DescriptorComperator) leaf.get()).getRawValue());
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
		DescriptorMathTree measure;

		final SelectQuery query = q("select timeseries of count(PERSON) AS PERSON from testPersonModel");
		final List<DescriptorMathTree> measures = query.getMeasures();
		assertEquals(1, measures.size());

		// check the different measure
		measure = measures.get(0);
		assertEquals("PERSON", measure.getId());
		assertTrue(measure.isSimple());

		// check the node function
		final MathOperatorNode node = (MathOperatorNode) measure.getRoot()
				.getChild(0);
		assertTrue(node.get().getFunction() instanceof Count);
		assertNull(node.get().getOperator());

		// check the person
		assertEquals(mathDescLeaf, node.getChild(0).getClass());
	}

	/**
	 * Tests the parsing of a {@code SelectQuery} with multiple measures.
	 */
	@Test
	public void testMultipleSimpleMeasure() {
		DescriptorMathTree measure;
		MathOperatorNode node;

		final SelectQuery query = q("select timeseries of Mean(P) AS AVG, min(A) AS MIN000, max(L) AS \"MAX\" from testPersonModel");
		final List<DescriptorMathTree> measures = query.getMeasures();
		assertEquals(3, measures.size());

		// check the different measure
		measure = measures.get(0);
		assertTrue(measure.isSimple());
		assertEquals("AVG", measure.getId());
		node = (MathOperatorNode) measure.getRoot().getChild(0);
		assertTrue(node.get().getFunction() instanceof Mean);

		measure = measures.get(1);
		assertTrue(measure.isSimple());
		assertEquals("MIN000", measure.getId());
		node = (MathOperatorNode) measure.getRoot().getChild(0);
		assertTrue(node.get().getFunction() instanceof Min);

		measure = measures.get(2);
		assertTrue(measure.isSimple());
		assertEquals("MAX", measure.getId());
		node = (MathOperatorNode) measure.getRoot().getChild(0);
		assertTrue(node.get().getFunction() instanceof Max);
	}

	/**
	 * Tests the arithmetic used within facts (i.e. multiplication before
	 * addition, ...).
	 */
	@Test
	public void testArithmeticFactMeasure() {
		SelectQuery query;
		List<DescriptorMathTree> measures;
		DescriptorMathTree measure;
		List<IMathTreeElement> order;
		MathOperator mo;

		/*
		 * Multplication prior to addition
		 */
		query = q("select timeseries of max(A + B * C) from testPersonModel");
		measures = query.getMeasures();
		assertEquals(1, measures.size());
		measure = measures.get(0);
		assertFalse(measure.isSimple());
		order = measure.getEvaluationOrder();

		// check the amount of commands
		assertEquals(order.toString(), 6, order.size());

		// check the types of commands
		assertEquals(order.toString(), mathDescLeaf, order.get(0).getClass());
		assertEquals(order.toString(), mathDescLeaf, order.get(1).getClass());
		assertEquals(order.toString(), mathDescLeaf, order.get(2).getClass());
		assertEquals(order.toString(), MathOperatorNode.class, order.get(3)
				.getClass());
		assertEquals(order.toString(), MathOperatorNode.class, order.get(4)
				.getClass());
		assertEquals(order.toString(), MathOperatorNode.class, order.get(5)
				.getClass());

		// check the order of the operators
		mo = ((MathOperatorNode) order.get(3)).get();
		assertEquals(ArithmeticOperator.MULTIPLY, mo.getOperator());
		assertNull(mo.getFunction());
		mo = ((MathOperatorNode) order.get(4)).get();
		assertEquals(ArithmeticOperator.ADD, mo.getOperator());
		assertNull(mo.getFunction());
		mo = ((MathOperatorNode) order.get(5)).get();
		assertTrue(mo.getFunction() instanceof Max);
		assertNull(mo.getOperator());

		/*
		 * check brackets usage
		 */
		query = q("select timeseries of max((A + B) * C) from testPersonModel");
		measures = query.getMeasures();
		assertEquals(1, measures.size());
		measure = measures.get(0);
		assertFalse(measure.isSimple());
		order = measure.getEvaluationOrder();

		// check the amount of commands
		assertEquals(6, order.size());

		// check the types of commands
		assertEquals(order.toString(), mathDescLeaf, order.get(0).getClass());
		assertEquals(order.toString(), mathDescLeaf, order.get(1).getClass());
		assertEquals(order.toString(), MathOperatorNode.class, order.get(2)
				.getClass());
		assertEquals(order.toString(), mathDescLeaf, order.get(3).getClass());
		assertEquals(order.toString(), MathOperatorNode.class, order.get(4)
				.getClass());
		assertEquals(order.toString(), MathOperatorNode.class, order.get(5)
				.getClass());

		// check the order of the operators
		mo = ((MathOperatorNode) order.get(2)).get();
		assertEquals(ArithmeticOperator.ADD, mo.getOperator());
		assertNull(mo.getFunction());
		mo = ((MathOperatorNode) order.get(4)).get();
		assertEquals(ArithmeticOperator.MULTIPLY, mo.getOperator());
		assertNull(mo.getFunction());
		mo = ((MathOperatorNode) order.get(5)).get();
		assertTrue(mo.getFunction() instanceof Max);
		assertNull(mo.getOperator());

		/*
		 * check multiple values within
		 */
		query = q("select timeseries of max((A + B - E) * C - D * E + F) from testPersonModel");
		measures = query.getMeasures();
		assertEquals(1, measures.size());
		measure = measures.get(0);
		assertFalse(measure.isSimple());
		order = measure.getEvaluationOrder();

		// check the amount of commands
		assertEquals(14, order.size());

		// check the types of commands
		assertEquals(order.toString(), mathDescLeaf, order.get(0).getClass());
		assertEquals(order.toString(), mathDescLeaf, order.get(1).getClass());
		assertEquals(order.toString(), MathOperatorNode.class, order.get(2)
				.getClass());
		assertEquals(order.toString(), mathDescLeaf, order.get(3).getClass());
		assertEquals(order.toString(), MathOperatorNode.class, order.get(4)
				.getClass());
		assertEquals(order.toString(), mathDescLeaf, order.get(5).getClass());
		assertEquals(order.toString(), MathOperatorNode.class, order.get(6)
				.getClass());
		assertEquals(order.toString(), mathDescLeaf, order.get(7).getClass());
		assertEquals(order.toString(), mathDescLeaf, order.get(8).getClass());
		assertEquals(order.toString(), MathOperatorNode.class, order.get(9)
				.getClass());
		assertEquals(order.toString(), MathOperatorNode.class, order.get(10)
				.getClass());
		assertEquals(order.toString(), mathDescLeaf, order.get(11).getClass());
		assertEquals(order.toString(), MathOperatorNode.class, order.get(12)
				.getClass());
		assertEquals(order.toString(), MathOperatorNode.class, order.get(13)
				.getClass());

		// check the order of the operators
		mo = ((MathOperatorNode) order.get(2)).get();
		assertEquals(ArithmeticOperator.ADD, mo.getOperator());
		assertNull(mo.getFunction());
		mo = ((MathOperatorNode) order.get(4)).get();
		assertEquals(ArithmeticOperator.MINUS, mo.getOperator());
		assertNull(mo.getFunction());
		mo = ((MathOperatorNode) order.get(6)).get();
		assertEquals(ArithmeticOperator.MULTIPLY, mo.getOperator());
		assertNull(mo.getFunction());
		mo = ((MathOperatorNode) order.get(9)).get();
		assertEquals(ArithmeticOperator.MULTIPLY, mo.getOperator());
		assertNull(mo.getFunction());
		mo = ((MathOperatorNode) order.get(10)).get();
		assertEquals(ArithmeticOperator.MINUS, mo.getOperator());
		assertNull(mo.getFunction());
		mo = ((MathOperatorNode) order.get(12)).get();
		assertEquals(ArithmeticOperator.ADD, mo.getOperator());
		assertNull(mo.getFunction());
		mo = ((MathOperatorNode) order.get(13)).get();
		assertTrue(mo.getFunction() instanceof Max);
		assertNull(mo.getOperator());
	}

	/**
	 * Tests the parsing of a more complex measures-definition.
	 */
	@Test
	public void testComplexMeasure() {
		DescriptorMathTree measure;
		List<IMathTreeElement> order;
		MathOperator mo;

		final SelectQuery query = q("select timeseries of mean(A + B * C), max(LOCATION + PERSON - WHAT) * (mean(PERSON) + min(PERSON)) from testPersonModel");
		final List<DescriptorMathTree> measures = query.getMeasures();
		assertFalse(query.usesFunction(IDimAggregationFunction.class));
		assertFalse(query.usesFunction(IMathAggregationFunction.class));
		assertTrue(query.usesFunction(ILowAggregationFunction.class));

		assertEquals(2, measures.size());

		/*
		 * check mean(A + B * C)
		 */
		measure = measures.get(0);
		assertFalse(measure.isSimple());
		order = measure.getEvaluationOrder();

		// check the amount of commands
		assertEquals(6, order.size());

		/*
		 * check max(LOCATION + PERSON - WHAT) * (Mean(PERSON) + min(PERSON))
		 */
		measure = measures.get(1);
		assertFalse(measure.isSimple());
		order = measure.getEvaluationOrder();

		// check the amount of commands
		assertEquals(12, order.size());

		// check the order of the operators
		mo = ((MathOperatorNode) order.get(2)).get();
		assertEquals(ArithmeticOperator.ADD, mo.getOperator());
		assertNull(mo.getFunction());

		mo = ((MathOperatorNode) order.get(4)).get();
		assertEquals(ArithmeticOperator.MINUS, mo.getOperator());
		assertNull(mo.getFunction());

		mo = ((MathOperatorNode) order.get(5)).get();
		assertNull(mo.getOperator());
		assertTrue(mo.getFunction() instanceof Max);
		assertEquals(ILowAggregationFunction.class, mo.getFunction()
				.getDefinedType());

		mo = ((MathOperatorNode) order.get(7)).get();
		assertNull(mo.getOperator());
		assertTrue(mo.getFunction() instanceof Mean);
		assertEquals(ILowAggregationFunction.class, mo.getFunction()
				.getDefinedType());

		mo = ((MathOperatorNode) order.get(9)).get();
		assertNull(mo.getOperator());
		assertTrue(mo.getFunction() instanceof Min);
		assertEquals(ILowAggregationFunction.class, mo.getFunction()
				.getDefinedType());

		mo = ((MathOperatorNode) order.get(10)).get();
		assertEquals(ArithmeticOperator.ADD, mo.getOperator());
		assertNull(mo.getFunction());

		mo = ((MathOperatorNode) order.get(11)).get();
		assertEquals(ArithmeticOperator.MULTIPLY, mo.getOperator());
		assertNull(mo.getFunction());
	}

	/**
	 * Tests the parsing of a measure using the selection of a level of a
	 * dimension
	 */
	@Test
	public void testDimAggregateMeasure() {
		DescriptorMathTree measure;
		List<IMathTreeElement> order;
		MathOperator mo;

		SelectQuery query;
		List<DescriptorMathTree> measures;

		query = q("select timeseries of SUM(COSTS) / COUNT(MINUTES) AS SAMPLE1, MODE(COSTS) * (SUM(COSTS + MINUTES) / COUNT(MINUTES)) AS SAMPLE2, COUNT(MINUTES) AS SAMPLE3 ON TIME.HIERARCHY.LEVEL from testPersonModel");
		assertTrue(query.usesFunction(IDimAggregationFunction.class));
		assertFalse(query.usesFunction(IMathAggregationFunction.class));
		assertFalse(query.usesFunction(ILowAggregationFunction.class));

		measures = query.getMeasures();
		assertEquals(3, measures.size());
		assertEquals("TIME", query.getMeasureDimension().getDimensionId());
		assertEquals("HIERARCHY", query.getMeasureDimension().getHierarchyId());
		assertEquals("LEVEL", query.getMeasureDimension().getLevelId());

		measure = measures.get(0);
		assertEquals("SAMPLE1", measure.getId());
		assertFalse(measure.isSimple());
		order = measure.getEvaluationOrder();
		assertEquals(5, order.size());

		mo = ((MathOperatorNode) order.get(1)).get();
		assertNull(mo.getOperator());
		assertTrue(mo.getFunction() instanceof Sum);
		assertEquals(IDimAggregationFunction.class, mo.getFunction()
				.getDefinedType());

		mo = ((MathOperatorNode) order.get(3)).get();
		assertNull(mo.getOperator());
		assertTrue(mo.getFunction() instanceof Count);
		assertEquals(IDimAggregationFunction.class, mo.getFunction()
				.getDefinedType());

		measure = measures.get(1);
		assertEquals("SAMPLE2", measure.getId());
		assertFalse(measure.isSimple());
		order = measure.getEvaluationOrder();
		assertEquals(10, order.size());

		mo = ((MathOperatorNode) order.get(1)).get();
		assertNull(mo.getOperator());
		assertTrue(mo.getFunction() instanceof Mode);
		assertEquals(IDimAggregationFunction.class, mo.getFunction()
				.getDefinedType());

		mo = ((MathOperatorNode) order.get(5)).get();
		assertNull(mo.getOperator());
		assertTrue(mo.getFunction() instanceof Sum);
		assertEquals(IDimAggregationFunction.class, mo.getFunction()
				.getDefinedType());

		mo = ((MathOperatorNode) order.get(7)).get();
		assertNull(mo.getOperator());
		assertTrue(mo.getFunction() instanceof Count);
		assertEquals(IDimAggregationFunction.class, mo.getFunction()
				.getDefinedType());

		measure = measures.get(2);
		assertEquals("SAMPLE3", measure.getId());
		assertTrue(measure.isSimple());
		order = measure.getEvaluationOrder();
		assertEquals(2, order.size());

		mo = ((MathOperatorNode) order.get(1)).get();
		assertNull(mo.getOperator());
		assertTrue(mo.getFunction() instanceof Count);
		assertEquals(IDimAggregationFunction.class, mo.getFunction()
				.getDefinedType());
	}

	/**
	 * Tests the parsing of a math-aggregate.
	 */
	@Test
	public void testMathAggregateMeasure() {
		DescriptorMathTree measure;
		List<IMathTreeElement> order;
		MathOperator mo;

		SelectQuery query;
		List<DescriptorMathTree> measures;

		query = q("select timeseries of MAX(SUM(PERSON)) AS SAMPLE0, MAX(SUM(COSTS) / COUNT(MINUTES)) AS SAMPLE1, MIN(COUNT(MINUTES)) + MAX(SUM(COSTS) / MIN(MINUTES)) AS SAMPLE2 ON TIME.HIERARCHY.LEVEL from testPersonModel");
		assertFalse(query.usesFunction(IDimAggregationFunction.class));
		assertTrue(query.usesFunction(IMathAggregationFunction.class));
		assertTrue(query.usesFunction(ILowAggregationFunction.class));

		measures = query.getMeasures();
		assertEquals(3, measures.size());
		assertEquals("TIME", query.getMeasureDimension().getDimensionId());
		assertEquals("HIERARCHY", query.getMeasureDimension().getHierarchyId());
		assertEquals("LEVEL", query.getMeasureDimension().getLevelId());

		measure = measures.get(0);
		assertEquals("SAMPLE0", measure.getId());
		assertFalse(measure.isSimple());
		order = measure.getEvaluationOrder();

		mo = ((MathOperatorNode) order.get(1)).get();
		assertNull(mo.getOperator());
		assertTrue(mo.getFunction() instanceof Sum);
		assertEquals(ILowAggregationFunction.class, mo.getFunction()
				.getDefinedType());

		mo = ((MathOperatorNode) order.get(2)).get();
		assertNull(mo.getOperator());
		assertTrue(mo.getFunction() instanceof Max);
		assertEquals(IMathAggregationFunction.class, mo.getFunction()
				.getDefinedType());

		measure = measures.get(1);
		assertEquals("SAMPLE1", measure.getId());
		assertFalse(measure.isSimple());
		order = measure.getEvaluationOrder();

		mo = ((MathOperatorNode) order.get(1)).get();
		assertNull(mo.getOperator());
		assertTrue(mo.getFunction() instanceof Sum);
		assertEquals(ILowAggregationFunction.class, mo.getFunction()
				.getDefinedType());

		mo = ((MathOperatorNode) order.get(3)).get();
		assertNull(mo.getOperator());
		assertTrue(mo.getFunction() instanceof Count);
		assertEquals(ILowAggregationFunction.class, mo.getFunction()
				.getDefinedType());

		mo = ((MathOperatorNode) order.get(4)).get();
		assertEquals(ArithmeticOperator.DIVIDE, mo.getOperator());
		assertNull(mo.getFunction());

		mo = ((MathOperatorNode) order.get(5)).get();
		assertNull(mo.getOperator());
		assertTrue(mo.getFunction() instanceof Max);
		assertEquals(IMathAggregationFunction.class, mo.getFunction()
				.getDefinedType());

		measure = measures.get(2);
		assertEquals("SAMPLE2", measure.getId());
		assertFalse(measure.isSimple());
		order = measure.getEvaluationOrder();
		mo = ((MathOperatorNode) order.get(1)).get();
		assertNull(mo.getOperator());
		assertTrue(mo.getFunction() instanceof Count);
		assertEquals(ILowAggregationFunction.class, mo.getFunction()
				.getDefinedType());

		mo = ((MathOperatorNode) order.get(2)).get();
		assertNull(mo.getOperator());
		assertTrue(mo.getFunction() instanceof Min);
		assertEquals(IMathAggregationFunction.class, mo.getFunction()
				.getDefinedType());

		mo = ((MathOperatorNode) order.get(4)).get();
		assertNull(mo.getOperator());
		assertTrue(mo.getFunction() instanceof Sum);
		assertEquals(ILowAggregationFunction.class, mo.getFunction()
				.getDefinedType());

		mo = ((MathOperatorNode) order.get(6)).get();
		assertNull(mo.getOperator());
		assertTrue(mo.getFunction() instanceof Min);
		assertEquals(ILowAggregationFunction.class, mo.getFunction()
				.getDefinedType());

		mo = ((MathOperatorNode) order.get(7)).get();
		assertEquals(ArithmeticOperator.DIVIDE, mo.getOperator());
		assertNull(mo.getFunction());

		mo = ((MathOperatorNode) order.get(8)).get();
		assertNull(mo.getOperator());
		assertTrue(mo.getFunction() instanceof Max);
		assertEquals(IMathAggregationFunction.class, mo.getFunction()
				.getDefinedType());

		mo = ((MathOperatorNode) order.get(9)).get();
		assertEquals(ArithmeticOperator.ADD, mo.getOperator());
		assertNull(mo.getFunction());
	}

	/**
	 * Tests the usage of a mixed measure using math- and dim-aggregates.
	 */
	@Test
	public void testMixedDimAndMath() {

		DescriptorMathTree measure;
		List<IMathTreeElement> order;
		MathOperator mo;

		SelectQuery query;
		List<DescriptorMathTree> measures;

		query = q("select timeseries of COUNT(PERSON) * MAX(MIN(PERSON)) AS SAMPLE0 ON TIME.HIERARCHY.LEVEL from testPersonModel");
		measures = query.getMeasures();

		assertTrue(query.usesFunction(IDimAggregationFunction.class));
		assertTrue(query.usesFunction(IMathAggregationFunction.class));
		assertTrue(query.usesFunction(ILowAggregationFunction.class));

		assertEquals(1, measures.size());
		assertEquals("TIME", query.getMeasureDimension().getDimensionId());
		assertEquals("HIERARCHY", query.getMeasureDimension().getHierarchyId());
		assertEquals("LEVEL", query.getMeasureDimension().getLevelId());

		measure = measures.get(0);
		assertEquals("SAMPLE0", measure.getId());
		assertFalse(measure.isSimple());
		order = measure.getEvaluationOrder();

		mo = ((MathOperatorNode) order.get(1)).get();
		assertNull(mo.getOperator());
		assertTrue(mo.getFunction() instanceof Count);
		assertEquals(IDimAggregationFunction.class, mo.getFunction()
				.getDefinedType());

		mo = ((MathOperatorNode) order.get(3)).get();
		assertNull(mo.getOperator());
		assertTrue(mo.getFunction() instanceof Min);
		assertEquals(ILowAggregationFunction.class, mo.getFunction()
				.getDefinedType());

		mo = ((MathOperatorNode) order.get(4)).get();
		assertNull(mo.getOperator());
		assertTrue(mo.getFunction() instanceof Max);
		assertEquals(IMathAggregationFunction.class, mo.getFunction()
				.getDefinedType());
	}

	/**
	 * Tests the retrieval of a simple group.
	 */
	@Test
	public void testParsingOfNoGroupBy() {
		final SelectQuery query = q("select timeSeries from \"model\" in [03.03.2014,05.03.2014)");
		final GroupExpression group = query.getGroup();

		final Set<Object> descriptors = group.getSelectors();
		assertEquals(0, descriptors.size());
	}

	/**
	 * Tests the retrieval of a simple group.
	 */
	@Test
	public void testParsingOfSimpleGroupBy() {
		final SelectQuery query = q("select timeSeries from \"model\" in [03.03.2014,05.03.2014) group by first, second, third");
		final GroupExpression group = query.getGroup();

		final Set<Object> descriptors = group.getSelectors();
		assertEquals(3, descriptors.size());
		final Iterator<Object> it = descriptors.iterator();
		assertEquals("first", it.next());
		assertEquals("second", it.next());
		assertEquals("third", it.next());
	}

	/**
	 * Tests the retrieval of a group with defined exclusions.
	 */
	@Test
	public void testParsingOfGroupByWithExclusion() {
		final SelectQuery query = q("select timeSeries from \"model\" in [03.03.2014,05.03.2014) group by first, second, third exclude {('\\\\hallo\\\\','pleasematch*','formula is a \\* b')}");
		final GroupExpression group = query.getGroup();

		final Set<Object> descriptors = group.getSelectors();
		assertEquals(3, descriptors.size());
		final Iterator<Object> itDesc = descriptors.iterator();
		assertEquals("first", itDesc.next());
		assertEquals("second", itDesc.next());
		assertEquals("third", itDesc.next());

		assertEquals(0, group.getInclusions().size());

		assertEquals(1, group.getExclusions().size());
		final List<DescriptorValue> excls = group.getExclusions().iterator()
				.next().getValues();
		assertEquals(3, excls.size());
		assertEquals("\\hallo\\", excls.get(0).getValue());
		assertEquals("pleasematch.*", excls.get(1).getValue());
		assertEquals("formula is a * b", excls.get(2).getValue());
	}

	/**
	 * Tests the retrieval of a group with defined inclusions.
	 */
	@Test
	public void testParsingOfGroupByWithInclusion() {
		final SelectQuery query = q("select timeSeries from \"model\" in [03.03.2014,05.03.2014) group by first, second, third include {('\\\\hallo\\\\','pleasematch*','formula is a \\* b')}");
		final GroupExpression group = query.getGroup();

		assertEquals(0, group.getExclusions().size());

		assertEquals(1, group.getInclusions().size());
		final List<DescriptorValue> excls = group.getInclusions().iterator()
				.next().getValues();
		assertEquals(3, excls.size());
		assertEquals("\\hallo\\", excls.get(0).getValue());
		assertEquals("pleasematch.*", excls.get(1).getValue());
		assertEquals("formula is a * b", excls.get(2).getValue());
	}

	/**
	 * Tests the usage of inclusions and exclusions
	 */
	@Test
	public void testParsingOfGroupByWithExAndInclusion() {
		final SelectQuery query = q("select timeSeries from \"model\" in [03.03.2014,05.03.2014) group by first, second, third include {('W*','matcher*s','*')} exclude {('A*', 'B')}");
		final GroupExpression group = query.getGroup();

		assertEquals(1, group.getExclusions().size());
		assertEquals(1, group.getInclusions().size());

		List<DescriptorValue> vals;
		vals = group.getInclusions().iterator().next().getValues();
		assertEquals(3, vals.size());
		assertEquals("W.*", vals.get(0).getValue());
		assertEquals("matcher.*s", vals.get(1).getValue());
		assertEquals(".*", vals.get(2).getValue());

		vals = group.getExclusions().iterator().next().getValues();
		assertEquals(2, vals.size());
		assertEquals("A.*", vals.get(0).getValue());
		assertEquals("B", vals.get(1).getValue());
	}

	/**
	 * Tests the parsing of a dimensional expression within group-by.
	 */
	@Test
	public void testParsingOfSimpleDimensionalGroupBy() {

		// check just one dimensional expression
		SelectQuery query = q("select timeSeries from \"model\" in [03.03.2014,05.03.2014) group by DIM.HIER.LVL");
		GroupExpression group = query.getGroup();

		Set<Object> selectors = group.getSelectors();
		assertEquals(1, selectors.size());
		Iterator<Object> it = selectors.iterator();
		assertEquals(new DimensionSelector("DIM", "HIER", "LVL"), it.next());

		// check a mixture
		query = q("select timeSeries from \"model\" in [03.03.2014,05.03.2014) group by PERSON, DIM2.HIER.LVL");
		group = query.getGroup();

		selectors = group.getSelectors();
		assertEquals(2, selectors.size());
		it = selectors.iterator();
		assertEquals("PERSON", it.next());
		assertEquals(new DimensionSelector("DIM2", "HIER", "LVL"), it.next());
	}

	/**
	 * Tests the logical linkage using {@code and} and {@code or}
	 */
	@Test
	public void testLogicalSimpleLinkage() {
		final SelectQuery query = q("select timeSeries from \"model\" in [03.03.2014,05.03.2014) filter by first='firstValue' AND second='secondValue' OR third='thirdValue'");
		final DescriptorLogicTree tree = query.getFilter();

		final List<ILogicalTreeElement> order = tree.getEvaluationOrder();

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
		assertEquals("first", ((DescriptorComperator) first.get()).getId());
		assertEquals("firstValue",
				((DescriptorComperator) first.get()).getRawValue());
		assertEquals("second", ((DescriptorComperator) second.get()).getId());
		assertEquals("secondValue",
				((DescriptorComperator) second.get()).getRawValue());
		assertEquals("third", ((DescriptorComperator) third.get()).getId());
		assertEquals("thirdValue",
				((DescriptorComperator) third.get()).getRawValue());

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
		final SelectQuery query = q("select timeseries from \"model\" in [03.03.2014,05.03.2014) filter by NOT HALLO='500' AND !HELLO='LALA'");
		final DescriptorLogicTree tree = query.getFilter();

		final List<ILogicalTreeElement> order = tree.getEvaluationOrder();

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
		assertEquals("HALLO", ((DescriptorComperator) hallo.get()).getId());
		assertEquals("500", ((DescriptorComperator) hallo.get()).getRawValue());
		assertEquals("HELLO", ((DescriptorComperator) hello.get()).getId());
		assertEquals("LALA", ((DescriptorComperator) hello.get()).getRawValue());

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
		final SelectQuery query = q("select timeseries from \"model\" in [03.03.2014,05.03.2014) filter by (!(CAKE='APPLE' OR NOT (CAKE='CHERRY')) AND !GREETINGS='HI')");
		final DescriptorLogicTree tree = query.getFilter();

		final List<ILogicalTreeElement> order = tree.getEvaluationOrder();

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

		final List<ILogicalTreeElement> order = tree.getEvaluationOrder();

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
		final List<ILogicalTreeElement> order = tree.getEvaluationOrder();
		assertEquals(1, order.size());

		final Object o = order.iterator().next();
		assertTrue(o instanceof DescriptorLeaf);
		assertEquals("FROM",
				((DescriptorComperator) ((DescriptorLeaf) o).get()).getId());
	}

	/**
	 * Tests the logical optimization for a simple query.
	 */
	@Test
	public void testLogicalSimpleOptimization() {
		final SelectQuery query = q("select timeseries from \"model\" in [03.03.2014,05.03.2014) filter by first='1' AND second='2' AND third='3'");
		final DescriptorLogicTree tree = query.getFilter();

		final List<ILogicalTreeElement> order = tree.getEvaluationOrder();

		assertEquals(4, order.size());
	}

	/**
	 * Tests the optimization for a compley query.
	 */
	@Test
	public void testLogicalComplexOptimization() {
		final SelectQuery query = q("select timeseries from \"model\" in [03.03.2014,05.03.2014) filter by (first='1' OR second='2') AND (third='3' OR firth='4') AND fifth='5' AND (sixth='6' OR (seventh='7' AND eight='8'))");
		final DescriptorLogicTree tree = query.getFilter();

		final List<ILogicalTreeElement> order = tree.getEvaluationOrder();

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
		thrown.expectMessage("syntax error was found at 1:58");

		q("select timeseries from \"model\" in [03.03.2014,05.03.2014) select timeseries from \"model\" in [03.03.2014,05.03.2014)");
	}

	/**
	 * Tests the exception to be thrown when concatenating select-statements.
	 */
	@Test
	public void testNestedSelectsException() {
		thrown.expect(QueryParsingException.class);
		thrown.expectMessage("syntax error was found at 1:23");

		q("select timeseries from (select timeseries from \"model\" in [03.03.2014,05.03.2014)) in [03.03.2014,05.03.2014)");
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
				null);

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
				null);

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
				null);

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
		final String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014) filter by SCREAMS='-1' OR PERSON='Philipp'";

		// load the model
		m(xml);

		// fire the query
		final SelectResult res = (SelectResult) factory.evaluateQuery(q(query),
				null);

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
		final String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014) filter by SCREAMS='-1' AND PERSON='Philipp'";

		// load the model
		m(xml);

		// fire the query
		final SelectResult res = (SelectResult) factory.evaluateQuery(q(query),
				null);

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
				null);

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
				null);

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
				null);

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
				null);

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
				null);

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
				null);

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
				null);

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
		final String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014) group by PERSON exclude {('Philipp'), ('Tobias')}";

		// load the model
		m(xml);

		// fire the query
		final SelectResult res = (SelectResult) factory.evaluateQuery(q(query),
				null);

		// check the result's filter
		final GroupResult gRes = res.getGroupResult();
		assertNotNull(gRes);
		assertEquals(2, gRes.size());
		assertNotNull(gRes.getEntry("Debbie"));
		assertNotNull(gRes.getEntry("Edison"));
	}

	/**
	 * Tests a simple {@code group by} expression with some included values.
	 */
	@Test
	public void testGroupByWithInclusion() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014) group by PERSON include {('Philipp'), ('Tobias')}";

		// load the model
		m(xml);

		// fire the query
		final SelectResult res = (SelectResult) factory.evaluateQuery(q(query),
				null);

		// check the result's filter
		final GroupResult gRes = res.getGroupResult();
		assertNotNull(gRes);
		assertEquals(2, gRes.size());
		assertNotNull(gRes.getEntry("Philipp"));
		assertNotNull(gRes.getEntry("Tobias"));
	}

	/**
	 * Tests the combination of includes and excludes.
	 */
	@Test
	public void testGroupByWithInAndExclusion() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014) group by PERSON include {('*i*')} exclude {('Philipp'), ('Tobias')}";

		// load the model
		m(xml);

		// fire the query
		final SelectResult res = (SelectResult) factory.evaluateQuery(q(query),
				null);

		// check the result's filter
		final GroupResult gRes = res.getGroupResult();
		assertNotNull(gRes);
		assertEquals(2, gRes.size());
		assertNotNull(gRes.getEntry("Edison"));
		assertNotNull(gRes.getEntry("Debbie"));
	}

	/**
	 * Group-By of a dimensional expression with exclusion.
	 */
	@Test
	public void testGroupByWithDimensionsAndExclusion() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModelWithDim.xml";
		final String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014) group by PERSON.GENDER.GENDER, LOCATION.GEO.CONTINENT exclude {('*', 'UNKNOWN')}";

		// load the model
		m(xml);

		// fire the query
		final SelectResult res = (SelectResult) factory.evaluateQuery(q(query),
				null);

		// check the result's filter
		final GroupResult gRes = res.getGroupResult();
		assertNotNull(gRes);

		assertEquals(2, gRes.size());
		assertNotNull(gRes.getEntry("MALE", "Europe"));
		assertNotNull(gRes.getEntry("FEMALE", "Europe"));

		GroupResultEntry e = gRes.getEntry("MALE", "Europe");
		assertEquals(0, e.getBitmap().getIds()[0]);
		assertEquals(1, e.getBitmap().getIds()[1]);
		assertEquals(3, e.getBitmap().getIds()[2]);
		assertEquals(5, e.getBitmap().getIds()[3]);
		e = gRes.getEntry("FEMALE", "Europe");
		assertEquals(4, e.getBitmap().getIds()[0]);
	}

	/**
	 * Tests the selection of an invalid dimension.
	 */
	@Test
	public void testInvalidDimensionInGroupBy() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModelWithDim.xml";
		final String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014) group by UNKNOWNDIM.GENDER.GENDER";

		// load the model
		m(xml);

		// fire the query
		thrown.expect(GroupEvaluatorException.class);
		thrown.expectMessage("The dimension 'UNKNOWNDIM' is not defined");
		factory.evaluateQuery(q(query), null);
	}

	/**
	 * Tests the selection of an invalid hierarchy.
	 */
	@Test
	public void testInvalidHierarchyInGroupBy() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModelWithDim.xml";
		final String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014) group by PERSON.UNKNOWNHIERARCHY.GENDER";

		// load the model
		m(xml);

		// fire the query
		thrown.expect(GroupEvaluatorException.class);
		thrown.expectMessage("The hierarchy 'UNKNOWNHIERARCHY' is not defined");
		factory.evaluateQuery(q(query), null);
	}

	/**
	 * Tests the selection of an invalid level.
	 */
	@Test
	public void testInvalidLevelInGroupBy() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModelWithDim.xml";
		final String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014) group by PERSON.GENDER.UNKNOWNLEVEL";

		// load the model
		m(xml);

		// fire the query
		thrown.expect(GroupEvaluatorException.class);
		thrown.expectMessage("The level 'UNKNOWNLEVEL' is not defined");
		factory.evaluateQuery(q(query), null);
	}

	/**
	 * Group-By of a dimensional expression with exclusion.
	 */
	@Test
	public void testGroupByWithExclusionOfNull() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModelWithDim.xml";

		// load the model
		m(xml);

		// 1. fire and check the query (excluding null)
		String query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014) group by LOCATION exclude {(NULL)}";
		SelectResult res = (SelectResult) factory.evaluateQuery(q(query), null);
		GroupResult gRes = res.getGroupResult();
		assertNotNull(gRes);
		assertEquals(2, gRes.size());
		assertNotNull(gRes.getEntry("Mönchengladbach"));
		assertNotNull(gRes.getEntry("Aachen"));

		// 2. fire and check the query (including null)
		query = "select timeseries from testPersonModel in [03.03.2014,05.03.2014) group by LOCATION ";
		res = (SelectResult) factory.evaluateQuery(q(query), null);
		gRes = res.getGroupResult();
		assertNotNull(gRes);
		assertEquals(3, gRes.size());
		assertNotNull(gRes.getEntry((String) null));
		assertEquals(2, gRes.getEntry((String) null).getBitmap().getIds()[0]);
		assertNotNull(gRes.getEntry("Mönchengladbach"));
		assertNotNull(gRes.getEntry("Aachen"));
	}

	/**
	 * Tests the parsing of dimensional filters.
	 */
	@Test
	public void testTimeSeriesDimensionalParsing() {
		final SelectQuery query = q("SELECT TIMESERIES from modelId IN (15.06.2014 , 15.06.2015 20:10:11] WHERE DIM.HIER.LEVEL = '5'");
		assertEquals(SelectResultType.TIMESERIES, query.getResultType());

		final DescriptorLogicTree tree = query.getFilter();
		final List<ILogicalTreeElement> order = tree.getEvaluationOrder();
		assertEquals(1, order.size());
		assertTrue(order.get(0) instanceof DescriptorLeaf);

		final DescriptorLeaf leaf = (DescriptorLeaf) order.get(0);
		assertTrue(leaf.get() instanceof DimensionComperator);
		assertEquals("DIM", ((DimensionComperator) leaf.get()).getDimension()
				.getDimensionId());
		assertEquals("HIER", ((DimensionComperator) leaf.get()).getDimension()
				.getHierarchyId());
		assertEquals("LEVEL", ((DimensionComperator) leaf.get()).getDimension()
				.getLevelId());
	}

	/**
	 * Tests the parsing of dimensional filters.
	 */
	@Test
	public void testRecordsDimensionalParsing() {
		final SelectQuery query = q("SELECT RECORDS from modelId WHERE DIM.HIER.LEVEL = '5' AND DIM2.HIER2.LEVEL2 = '6'");
		assertEquals(SelectResultType.RECORDS, query.getResultType());

		final DescriptorLogicTree tree = query.getFilter();
		final List<ILogicalTreeElement> order = tree.getEvaluationOrder();
		assertEquals(3, order.size());

		assertTrue(order.get(0) instanceof DescriptorLeaf);
		DescriptorLeaf leaf = (DescriptorLeaf) order.get(0);
		assertTrue(leaf.get() instanceof DimensionComperator);
		assertEquals("DIM", ((DimensionComperator) leaf.get()).getDimension()
				.getDimensionId());
		assertEquals("HIER", ((DimensionComperator) leaf.get()).getDimension()
				.getHierarchyId());
		assertEquals("LEVEL", ((DimensionComperator) leaf.get()).getDimension()
				.getLevelId());

		assertTrue(order.get(1) instanceof DescriptorLeaf);
		leaf = (DescriptorLeaf) order.get(1);
		assertTrue(leaf.get() instanceof DimensionComperator);
		assertEquals("DIM2", ((DimensionComperator) leaf.get()).getDimension()
				.getDimensionId());
		assertEquals("HIER2", ((DimensionComperator) leaf.get()).getDimension()
				.getHierarchyId());
		assertEquals("LEVEL2", ((DimensionComperator) leaf.get())
				.getDimension().getLevelId());
	}

	/**
	 * Tests the parsing of the limit operator.
	 */
	@Test
	public void testRecordsLimitOperator() {
		SelectQuery query;

		query = q("SELECT RECORDS from modelId WHERE VALUE='5' LIMIT 5, 6");
		assertEquals(SelectResultType.RECORDS, query.getResultType());
		assertEquals(query.getOffset(), 5);
		assertEquals(query.getLimit(), 6);

		query = q("SELECT RECORDS from modelId LIMIT 50, 60");
		assertEquals(SelectResultType.RECORDS, query.getResultType());
		assertEquals(query.getOffset(), 50);
		assertEquals(query.getLimit(), 60);
		assertTrue(query.hasLimit());

		query = q("SELECT RECORDS from modelId LIMIT 0, 100");
		assertEquals(SelectResultType.RECORDS, query.getResultType());
		assertEquals(query.getOffset(), 0);
		assertEquals(query.getLimit(), 100);
		assertTrue(query.hasLimit());

		query = q("SELECT RECORDS from modelId LIMIT 0, 100");
		assertEquals(SelectResultType.RECORDS, query.getResultType());
		assertEquals(query.getOffset(), 0);
		assertEquals(query.getLimit(), 100);
		assertTrue(query.hasLimit());

		query = q("SELECT RECORDS from modelId limit 100");
		assertEquals(SelectResultType.RECORDS, query.getResultType());
		assertEquals(query.getOffset(), 100);
		assertEquals(query.getLimit(), -1);
		assertTrue(query.hasLimit());

		query = q("SELECT RECORDS from modelId");
		assertEquals(SelectResultType.RECORDS, query.getResultType());
		assertEquals(query.getOffset(), 0);
		assertEquals(query.getLimit(), -1);
		assertFalse(query.hasLimit());
	}

	/**
	 * Tests the implementation of the {@link Count} aggregation-function.
	 */
	@Test
	public void testAggregationCount() {
		TimeSeries ts;

		final String xml = "/net/meisen/dissertation/impl/parser/query/testNumberModel.xml";
		final String query = "select timeseries of count(PAX) AS PAX, count(PAX + AIRLINE) AS COMPLEX_COUNT from testNumberModel in [01.01.2014,02.01.2014]";

		// load the model
		m(xml);

		// fire the query
		final SelectResultTimeSeries res = (SelectResultTimeSeries) factory
				.evaluateQuery(q(query), null);
		final TimeSeriesCollection tsRes = res.getTimeSeriesResult();
		assertEquals(2, tsRes.amountOfSeries());
		assertEquals("01.01.2014 00:00:00,000", tsRes.getLabel(0));
		assertEquals("02.01.2014 00:00:00,000", tsRes.getLabel(1));

		// check the results
		ts = tsRes.getSeries("PAX");
		assertNotNull(ts);
		assertEquals(ts.toString(), 0.0, ts.getValue(0), 0.0);
		assertEquals(ts.toString(), 2.0, ts.getValue(1), 0.0);

		ts = tsRes.getSeries("COMPLEX_COUNT");
		assertNotNull(ts);
		assertEquals(ts.toString(), 0.0, ts.getValue(0), 0.0);
		assertEquals(ts.toString(), 2.0, ts.getValue(1), 0.0);
	}

	/**
	 * Tests the retrieval of facts using no filter but a grouping expression.
	 */
	@Test
	public void testGroupByFactsCalculationWithoutFilter() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select timeseries of sum(SCREAMS) AS SCREAMS from testPersonModel in [03.03.2014 16:18:00,03.03.2014 16:21:00] group by LOCATION";

		// load the model
		m(xml);

		// fire the query
		final SelectResultTimeSeries res = (SelectResultTimeSeries) factory
				.evaluateQuery(q(query), null);
		final TimeSeriesCollection tsRes = res.getTimeSeriesResult();
		assertEquals(3, tsRes.size());

		final TimeSeries tsMg = tsRes.getSeries("Mönchengladbach (SCREAMS)");
		assertEquals(3.0, tsMg.getValue(0), 0.0);
		assertEquals(3.0, tsMg.getValue(1), 0.0);
		assertEquals(0.0, tsMg.getValue(2), 0.0);
		assertEquals(0.0, tsMg.getValue(3), 0.0);

		final TimeSeries tsAc = tsRes.getSeries("Aachen (SCREAMS)");
		assertEquals(12.0, tsAc.getValue(0), 0.0);
		assertEquals(12.0, tsAc.getValue(1), 0.0);
		assertEquals(12.0, tsAc.getValue(2), 0.0);
		assertEquals(12.0, tsAc.getValue(3), 0.0);

		final TimeSeries tsUn = tsRes.getSeries("Undefined (SCREAMS)");
		assertEquals(0.0, tsUn.getValue(0), 0.0);
		assertEquals(0.0, tsUn.getValue(1), 0.0);
		assertEquals(0.0, tsUn.getValue(2), 0.0);
		assertEquals(0.0, tsUn.getValue(3), 0.0);
	}

	/**
	 * Tests the retrieval of grouped values.
	 */
	@Test
	public void testGroupByFactsCalculationWithSeveralGroups() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select timeseries of max(SCREAMS) AS SCREAMS from testPersonModel in [03.03.2014 16:18:00,03.03.2014 16:21:00] group by LOCATION, PERSON EXCLUDE {('*', 'D*'), ('*', 'P*'), ('*', 'T*')}";

		// load the model
		m(xml);

		// fire the query
		final SelectResultTimeSeries res = (SelectResultTimeSeries) factory
				.evaluateQuery(q(query), null);
		final TimeSeriesCollection tsRes = res.getTimeSeriesResult();
		assertEquals(3, tsRes.size());

		final TimeSeries tsMg = tsRes
				.getSeries("Mönchengladbach, Edison (SCREAMS)");
		assertEquals(Double.NaN, tsMg.getValue(0), 0.0);
		assertEquals(Double.NaN, tsMg.getValue(1), 0.0);
		assertEquals(Double.NaN, tsMg.getValue(2), 0.0);
		assertEquals(Double.NaN, tsMg.getValue(3), 0.0);

		final TimeSeries tsAc = tsRes.getSeries("Aachen, Edison (SCREAMS)");
		assertEquals(12.0, tsAc.getValue(0), 0.0);
		assertEquals(12.0, tsAc.getValue(1), 0.0);
		assertEquals(12.0, tsAc.getValue(2), 0.0);
		assertEquals(12.0, tsAc.getValue(3), 0.0);

		final TimeSeries tsUn = tsRes.getSeries("Undefined, Edison (SCREAMS)");
		assertEquals(Double.NaN, tsUn.getValue(0), 0.0);
		assertEquals(Double.NaN, tsUn.getValue(1), 0.0);
		assertEquals(Double.NaN, tsUn.getValue(2), 0.0);
		assertEquals(Double.NaN, tsUn.getValue(3), 0.0);
	}

	/**
	 * Tests the retrieval of facts using a filter, a grouping expression and
	 * ignore.
	 */
	@Test
	public void testGroupByFactsCalculationWithFilter() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select timeseries of sum(SCREAMS) AS SCREAMS from testPersonModel in [03.03.2014 16:18:00,03.03.2014 16:21:00] filter by !LOCATION='Aachen' group by LOCATION EXCLUDE {('Undefined')}";

		// load the model
		m(xml);

		// fire the query
		final SelectResultTimeSeries res = (SelectResultTimeSeries) factory
				.evaluateQuery(q(query), null);
		final TimeSeriesCollection tsRes = res.getTimeSeriesResult();

		final TimeSeries tsMg = tsRes.getSeries("Mönchengladbach (SCREAMS)");
		assertEquals(3.0, tsMg.getValue(0), 0.0);
		assertEquals(3.0, tsMg.getValue(1), 0.0);
		assertEquals(0.0, tsMg.getValue(2), 0.0);
		assertEquals(0.0, tsMg.getValue(3), 0.0);

		final TimeSeries tsAc = tsRes.getSeries("Aachen (SCREAMS)");
		assertEquals(0.0, tsAc.getValue(0), 0.0);
		assertEquals(0.0, tsAc.getValue(1), 0.0);
		assertEquals(0.0, tsAc.getValue(2), 0.0);
		assertEquals(0.0, tsAc.getValue(3), 0.0);

		assertNull(tsRes.getSeries("Undefined (SCREAMS)"));
	}

	/**
	 * Tests the implementation of the {@link Mean} aggregation-function.
	 */
	@Test
	public void testAggregationMean() {
		TimeSeries ts;

		final String xml = "/net/meisen/dissertation/impl/parser/query/testNumberModel.xml";
		final String query = "select timeseries of mean(PAX) AS PAX, mean(PAX + PAX) AS COMPLEX_MEAN from testNumberModel in [01.01.2014,04.01.2014]";

		// load the model
		m(xml);

		// fire the query
		final SelectResultTimeSeries res = (SelectResultTimeSeries) factory
				.evaluateQuery(q(query), null);
		final TimeSeriesCollection tsRes = res.getTimeSeriesResult();
		assertEquals(2, tsRes.amountOfSeries());
		assertEquals("01.01.2014 00:00:00,000", tsRes.getLabel(0));
		assertEquals("02.01.2014 00:00:00,000", tsRes.getLabel(1));
		assertEquals("03.01.2014 00:00:00,000", tsRes.getLabel(2));
		assertEquals("04.01.2014 00:00:00,000", tsRes.getLabel(3));

		// check the results
		ts = tsRes.getSeries("PAX");
		assertNotNull(ts);
		ts.size();
		assertEquals(ts.toString(), 0.0, ts.getValue(0), 0.0);
		assertEquals(ts.toString(), 996.0, ts.getValue(1), 0.0);
		assertEquals(ts.toString(), 1200.0, ts.getValue(2), 0.0);
		assertEquals(ts.toString(), 1200.0, ts.getValue(3), 0.0);
		assertEquals(4, ts.size());

		ts = tsRes.getSeries("COMPLEX_MEAN");
		assertNotNull(ts);
		assertEquals(ts.toString(), 0.0, ts.getValue(0), 0.0);
		assertEquals(ts.toString(), 1992.0, ts.getValue(1), 0.0);
		assertEquals(ts.toString(), 2400.0, ts.getValue(2), 0.0);
		assertEquals(ts.toString(), 2400.0, ts.getValue(3), 0.0);
		assertEquals(4, ts.size());
	}

	/**
	 * Tests the implementation of the {@link Max} aggregation-function.
	 */
	@Test
	public void testAggregationMax() {
		TimeSeries ts;

		final String xml = "/net/meisen/dissertation/impl/parser/query/testNumberModel.xml";
		final String query = "select timeseries of max(PAX) AS PAX, max(PAX + AIRLINE) AS COMPLEX_MAX, max(PAX) + max(AIRLINE) AS COMBINED_MAX from testNumberModel in [01.01.2014,02.01.2014]";

		// load the model
		m(xml);

		// fire the query
		final SelectResultTimeSeries res = (SelectResultTimeSeries) factory
				.evaluateQuery(q(query), null);
		final TimeSeriesCollection tsRes = res.getTimeSeriesResult();
		assertEquals(3, tsRes.amountOfSeries());
		assertEquals("01.01.2014 00:00:00,000", tsRes.getLabel(0));
		assertEquals("02.01.2014 00:00:00,000", tsRes.getLabel(1));

		// check the results
		ts = tsRes.getSeries("PAX");
		assertNotNull(ts);
		assertEquals(ts.toString(), Double.NaN, ts.getValue(0), 0.0);
		assertEquals(ts.toString(), 1239.0, ts.getValue(1), 0.0);

		ts = tsRes.getSeries("COMPLEX_MAX");
		assertNotNull(ts);
		assertEquals(ts.toString(), Double.NaN, ts.getValue(0), 0.0);
		assertEquals(ts.toString(), 1240.0, ts.getValue(1), 0.0);

		ts = tsRes.getSeries("COMBINED_MAX");
		assertNotNull(ts);
		assertEquals(ts.toString(), Double.NaN, ts.getValue(0), 0.0);
		assertEquals(ts.toString(), 1240.0, ts.getValue(1), 0.0);
	}

	/**
	 * Tests the implementation of a aggregated count-started.
	 */
	@Test
	public void testAggregationCountStarted() {
		String query;
		TimeSeries ts;

		// load the model
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		m(xml);

		// fire a first query
		query = "select timeseries of countstarted(PERSON) AS CSTART from testPersonModel in [03.03.2014 16:19:00,03.03.2014 16:22:00]";
		ts = ((SelectResultTimeSeries) factory.evaluateQuery(q(query), null))
				.getTimeSeriesResult().getSeries("CSTART");
		int i1 = 0;
		for (final double value : ts.getValues()) {
			if (i1 == 1) {
				assertEquals(1.0, value, 0.0);
			} else {
				assertEquals(0.0, value, 0.0);
			}
			i1++;
		}

		// fire a second query
		query = "select timeseries of countstarted(PERSON) AS CSTART from testPersonModel in [03.03.2014 00:00:00,03.03.2014 16:20:00]";
		ts = ((SelectResultTimeSeries) factory.evaluateQuery(q(query), null))
				.getTimeSeriesResult().getSeries("CSTART");
		int i2 = 0;
		for (final double value : ts.getValues()) {
			if (i2 == 0) {
				assertEquals(4.0, value, 0.0);
			} else if (i2 == ts.getValues().length - 1) {
				assertEquals(1.0, value, 0.0);
			} else {
				assertEquals(0.0, value, 0.0);
			}
			i2++;
		}
	}

	/**
	 * Tests the implementation of a aggregated count-finished.
	 */
	@Test
	public void testAggregationCountFinished() {
		String query;
		TimeSeries ts;

		// load the model
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		m(xml);

		// fire a first query
		query = "select timeseries of countfinished(PERSON) AS CFINISHED from testPersonModel in [03.03.2014 16:18:00,03.03.2014 16:22:00]";
		ts = ((SelectResultTimeSeries) factory.evaluateQuery(q(query), null))
				.getTimeSeriesResult().getSeries("CFINISHED");
		int i1 = 0;
		for (final double value : ts.getValues()) {
			if (i1 == 1) {
				assertEquals(1.0, value, 0.0);
			} else {
				assertEquals(0.0, value, 0.0);
			}
			i1++;
		}

		// fire a second query
		query = "select timeseries of countfinished(PERSON) AS CFINISHED from testPersonModel in [03.03.2014 17:20:00,04.03.2014 23:59:00]";
		ts = ((SelectResultTimeSeries) factory.evaluateQuery(q(query), null))
				.getTimeSeriesResult().getSeries("CFINISHED");
		int i2 = 0;
		for (final double value : ts.getValues()) {
			if (i2 == 1) {
				assertEquals(1.0, value, 0.0);
			} else if (i2 == ts.getValues().length - 1) {
				assertEquals(4.0, value, 0.0);
			} else {
				assertEquals(0.0, value, 0.0);
			}
			i2++;
		}
	}

	/**
	 * Tests the implementation of the {@link Min} aggregation-function.
	 */
	@Test
	public void testAggregationMin() {
		TimeSeries ts;

		final String xml = "/net/meisen/dissertation/impl/parser/query/testNumberModel.xml";
		final String query = "select timeseries of min(PAX) AS PAX, min(PAX + CREW) AS COMPLEX_MIN, min(PAX) + min(CREW) AS COMBINED_MIN from testNumberModel in [05.01.2014,06.01.2014]";

		// load the model
		m(xml);

		// fire the query
		final SelectResultTimeSeries res = (SelectResultTimeSeries) factory
				.evaluateQuery(q(query), null);
		final TimeSeriesCollection tsRes = res.getTimeSeriesResult();
		assertEquals(3, tsRes.amountOfSeries());
		assertEquals("05.01.2014 00:00:00,000", tsRes.getLabel(0));
		assertEquals("06.01.2014 00:00:00,000", tsRes.getLabel(1));

		// check the results
		ts = tsRes.getSeries("PAX");
		assertNotNull(ts);
		assertEquals(ts.toString(), 3.0, ts.getValue(0), 0.0);
		assertEquals(ts.toString(), Double.NaN, ts.getValue(1), 0.0);

		ts = tsRes.getSeries("COMPLEX_MIN");
		assertNotNull(ts);
		assertEquals(ts.toString(), 9.0, ts.getValue(0), 0.0);
		assertEquals(ts.toString(), Double.NaN, ts.getValue(1), 0.0);

		ts = tsRes.getSeries("COMBINED_MIN");
		assertNotNull(ts);
		assertEquals(ts.toString(), 7.0, ts.getValue(0), 0.0);
		assertEquals(ts.toString(), Double.NaN, ts.getValue(1), 0.0);
	}

	/**
	 * Tests the implementation of the {@link Sum} aggregation-function.
	 */
	@Test
	public void testAggregationSum() {
		TimeSeries ts;

		final String xml = "/net/meisen/dissertation/impl/parser/query/testNumberModel.xml";
		final String query = "select timeseries of sum(PAX) AS PAX, sum(PAX + CREW) AS COMPLEX_SUM, sum(PAX) + sum(CREW) AS COMBINED_SUM from testNumberModel in [01.01.2014,02.01.2014]";

		// load the model
		m(xml);

		// fire the query
		final SelectResultTimeSeries res = (SelectResultTimeSeries) factory
				.evaluateQuery(q(query), null);
		final TimeSeriesCollection tsRes = res.getTimeSeriesResult();
		assertEquals(3, tsRes.amountOfSeries());
		assertEquals("01.01.2014 00:00:00,000", tsRes.getLabel(0));
		assertEquals("02.01.2014 00:00:00,000", tsRes.getLabel(1));

		// check the results
		ts = tsRes.getSeries("PAX");
		assertNotNull(ts);
		assertEquals(ts.toString(), 0.0, ts.getValue(0), 0.0);
		assertEquals(ts.toString(), 1992.0, ts.getValue(1), 0.0);

		ts = tsRes.getSeries("COMPLEX_SUM");
		assertNotNull(ts);
		assertEquals(ts.toString(), 0.0, ts.getValue(0), 0.0);
		assertEquals(ts.toString(), 2000.0, ts.getValue(1), 0.0);

		ts = tsRes.getSeries("COMBINED_SUM");
		assertNotNull(ts);
		assertEquals(ts.toString(), 0.0, ts.getValue(0), 0.0);
		assertEquals(ts.toString(), 2000.0, ts.getValue(1), 0.0);
	}

	/**
	 * Tests the implementation of {@link Median}.
	 */
	@Test
	public void testAggregationMedian() {
		TimeSeries ts;

		final String xml = "/net/meisen/dissertation/impl/parser/query/testNumberModel.xml";
		final String query = "select timeseries of median(PAX) AS PAX, median(CREW) AS CREW, median(PAX + CREW) AS COMPLEX_MEDIAN, median(PAX) + median(CREW) AS COMBINED_MEDIAN from testNumberModel in [08.01.2014,10.01.2014]";

		// load the model
		m(xml);

		// fire the query
		final SelectResultTimeSeries res = (SelectResultTimeSeries) factory
				.evaluateQuery(q(query), null);
		final TimeSeriesCollection tsRes = res.getTimeSeriesResult();
		assertEquals(4, tsRes.amountOfSeries());
		assertEquals("08.01.2014 00:00:00,000", tsRes.getLabel(0));
		assertEquals("09.01.2014 00:00:00,000", tsRes.getLabel(1));
		assertEquals("10.01.2014 00:00:00,000", tsRes.getLabel(2));

		// check the results
		ts = tsRes.getSeries("PAX");
		assertNotNull(ts);
		assertEquals(ts.toString(), 550.0, ts.getValue(0), 0.0);
		assertEquals(ts.toString(), 1000.0, ts.getValue(1), 0.0);
		assertEquals(ts.toString(), Double.NaN, ts.getValue(2), 0.0);

		ts = tsRes.getSeries("CREW");
		assertNotNull(ts);
		assertEquals(ts.toString(), 2.0, ts.getValue(0), 0.0);
		assertEquals(ts.toString(), 3.0, ts.getValue(1), 0.0);
		assertEquals(ts.toString(), Double.NaN, ts.getValue(2), 0.0);

		ts = tsRes.getSeries("COMPLEX_MEDIAN");
		assertNotNull(ts);
		assertEquals(ts.toString(), 552.0, ts.getValue(0), 0.0);
		assertEquals(ts.toString(), 1003.0, ts.getValue(1), 0.0);
		assertEquals(ts.toString(), Double.NaN, ts.getValue(2), 0.0);

		ts = tsRes.getSeries("COMBINED_MEDIAN");
		assertNotNull(ts);
		assertEquals(ts.toString(), 552.0, ts.getValue(0), 0.0);
		assertEquals(ts.toString(), 1003.0, ts.getValue(1), 0.0);
		assertEquals(ts.toString(), Double.NaN, ts.getValue(2), 0.0);
	}

	/**
	 * Tests the implementation of the {@code AggregatonFunction} {@link Mode}.
	 */
	@Test
	public void testAggregationMode() {
		TimeSeries ts;

		final String xml = "/net/meisen/dissertation/impl/parser/query/testNumberModel.xml";
		final String query = "select timeseries of mode(PAX) AS PAX, mode(CREW) AS CREW, mode(PAX + CREW) AS COMPLEX_MODE, mode(PAX) + mode(CREW) AS COMBINED_MODE from testNumberModel in [01.01.2014,05.01.2014]";

		// load the model
		m(xml);

		// fire the query
		final SelectResultTimeSeries res = (SelectResultTimeSeries) factory
				.evaluateQuery(q(query), null);
		final TimeSeriesCollection tsRes = res.getTimeSeriesResult();
		assertEquals(4, tsRes.amountOfSeries());
		assertEquals("01.01.2014 00:00:00,000", tsRes.getLabel(0));
		assertEquals("02.01.2014 00:00:00,000", tsRes.getLabel(1));
		assertEquals("03.01.2014 00:00:00,000", tsRes.getLabel(2));
		assertEquals("04.01.2014 00:00:00,000", tsRes.getLabel(3));
		assertEquals("05.01.2014 00:00:00,000", tsRes.getLabel(4));

		// check the results
		ts = tsRes.getSeries("PAX");
		assertNotNull(ts);
		assertEquals(ts.toString(), Double.NaN, ts.getValue(0), 0.0);
		assertEquals(ts.toString(), Double.NaN, ts.getValue(1), 0.0);
		assertEquals(ts.toString(), 1200.0, ts.getValue(2), 0.0);
		assertEquals(ts.toString(), 1200.0, ts.getValue(3), 0.0);
		assertEquals(ts.toString(), Double.NaN, ts.getValue(4), 0.0);

		ts = tsRes.getSeries("CREW");
		assertNotNull(ts);
		assertEquals(ts.toString(), Double.NaN, ts.getValue(0), 0.0);
		assertEquals(ts.toString(), 4.0, ts.getValue(1), 0.0);
		assertEquals(ts.toString(), 4.0, ts.getValue(2), 0.0);
		assertEquals(ts.toString(), 4.0, ts.getValue(3), 0.0);
		assertEquals(ts.toString(), Double.NaN, ts.getValue(4), 0.0);

		ts = tsRes.getSeries("COMPLEX_MODE");
		assertNotNull(ts);
		assertEquals(ts.toString(), Double.NaN, ts.getValue(0), 0.0);
		assertEquals(ts.toString(), Double.NaN, ts.getValue(1), 0.0);
		assertEquals(ts.toString(), 1204.0, ts.getValue(2), 0.0);
		assertEquals(ts.toString(), 1204.0, ts.getValue(3), 0.0);
		assertEquals(ts.toString(), Double.NaN, ts.getValue(4), 0.0);

		ts = tsRes.getSeries("COMBINED_MODE");
		assertNotNull(ts);
		assertEquals(ts.toString(), Double.NaN, ts.getValue(0), 0.0);
		assertEquals(ts.toString(), Double.NaN, ts.getValue(1), 0.0);
		assertEquals(ts.toString(), 1204.0, ts.getValue(2), 0.0);
		assertEquals(ts.toString(), 1204.0, ts.getValue(3), 0.0);
		assertEquals(ts.toString(), Double.NaN, ts.getValue(4), 0.0);
	}

	/**
	 * Tests the aggregation across a dimension.
	 */
	@Test
	public void testAggregationDim() {
		String query;
		TimeSeriesCollection tsRes;
		TimeSeries ts;

		// load the model
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModelWithDim.xml";
		m(xml);

		// fire the UTC query
		query = "select timeseries of COUNT(PERSON) AS PERSON, SUM(SCREAMS) AS SCREAMS, SUM(SCREAMS) / COUNT(PERSON) AS COMPLEX , SUM(SCREAMS + PERSON) AS COMBINED ON TIME.UTC.DAY from testPersonModel";
		tsRes = ((SelectResultTimeSeries) factory.evaluateQuery(q(query), null))
				.getTimeSeriesResult();

		ts = tsRes.getSeries("PERSON");
		assertEquals(6.0, ts.getValue(0), 0.0);
		assertEquals(4.0, ts.getValue(1), 0.0);

		ts = tsRes.getSeries("SCREAMS");
		assertEquals(15.0, ts.getValue(0), 0.0);
		assertEquals(12.0, ts.getValue(1), 0.0);

		ts = tsRes.getSeries("COMPLEX");
		assertEquals(15.0 / 6.0, ts.getValue(0), 0.0);
		assertEquals(12.0 / 4.0, ts.getValue(1), 0.0);

		ts = tsRes.getSeries("COMBINED");
		assertEquals(15.0 + 6.0, ts.getValue(0), 0.0);
		assertEquals(12.0 + 4.0, ts.getValue(1), 0.0);

		// test another hierarchy
		query = "select timeseries of COUNT(PERSON) AS PERSON ON TIME.GER.MINUTE30 from testPersonModel";
		tsRes = ((SelectResultTimeSeries) factory.evaluateQuery(q(query), null))
				.getTimeSeriesResult();
		ts = tsRes.getSeries("PERSON");
		for (int i = 0; i < ts.size(); i++) {
			if (i == 32 || i == 34) {
				assertEquals(5.0, ts.getValue(i), 0.0);
			} else {
				assertEquals(4.0, ts.getValue(i), 0.0);
			}
		}

		// test some filtering
		query = "select timeseries of COUNT(PERSON) AS PERSON, SUM(SCREAMS) AS SCREAMS ON TIME.US.HOUR2 from testPersonModel where PERSON='Philipp'";
		tsRes = ((SelectResultTimeSeries) factory.evaluateQuery(q(query), null))
				.getTimeSeriesResult();
		ts = tsRes.getSeries("PERSON");
		for (int i = 0; i < ts.size(); i++) {
			if (i == 8) {
				assertEquals(3.0, ts.getValue(i), 0.0);
			} else {
				assertEquals(1.0, ts.getValue(i), 0.0);
			}
		}

		ts = tsRes.getSeries("SCREAMS");
		for (int i = 0; i < ts.size(); i++) {
			if (i < 9) {
				assertEquals(3.0, ts.getValue(i), 0.0);
			} else {
				assertEquals(0.0, ts.getValue(i), 0.0);
			}
		}
	}

	/**
	 * Tests the math-aggregation.
	 */
	@Test
	public void testAggregationMath() {
		String query;
		TimeSeriesCollection tsRes;
		TimeSeries ts;

		// load the model
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModelWithDim.xml";
		m(xml);

		// fire the UTC query
		query = "select timeseries of MAX(SUM(SCREAMS)) AS RESNEED, MIN(SUM(SCREAMS)) AS MINRESNEED, MIN(COUNT(PERSON) * SUM(SCREAMS + LOCATION)) AS WEIRD ON TIME.UTC.DAY from testPersonModel";
		tsRes = ((SelectResultTimeSeries) factory.evaluateQuery(q(query), null))
				.getTimeSeriesResult();
		ts = tsRes.getSeries("RESNEED");
		assertEquals(2, ts.size());
		assertEquals(15.0, ts.getValue(0), 0.0);
		assertEquals(12.0, ts.getValue(1), 0.0);

		ts = tsRes.getSeries("MINRESNEED");
		assertEquals(2, ts.size());
		assertEquals(12.0, ts.getValue(0), 0.0);
		assertEquals(12.0, ts.getValue(1), 0.0);

		ts = tsRes.getSeries("WEIRD");
		assertEquals(2, ts.size());
		assertEquals(64.0, ts.getValue(0), 0.0);
		assertEquals(64.0, ts.getValue(1), 0.0);

		query = "select timeseries of MAX(SUM(SCREAMS)) AS RESNEED ON TIME.UTC.DAY from testPersonModel in [03.03.2014, 04.03.2014)";
		tsRes = ((SelectResultTimeSeries) factory.evaluateQuery(q(query), null))
				.getTimeSeriesResult();
		ts = tsRes.getSeries("RESNEED");
		assertEquals(1, ts.size());
		assertEquals(15.0, ts.getValue(0), 0.0);

		query = "select timeseries of MAX(SUM(SCREAMS)) AS RESNEED ON TIME.UTC.DAY from testPersonModel in [03.03.2014 17:00:00, 04.03.2014)";
		tsRes = ((SelectResultTimeSeries) factory.evaluateQuery(q(query), null))
				.getTimeSeriesResult();
		ts = tsRes.getSeries("RESNEED");
		assertEquals(1, ts.size());
		assertEquals(12.0, ts.getValue(0), 0.0);

		query = "select timeseries of SUM(COUNTSTARTED(PERSON)) AS SAMPLE ON TIME.UTC.DAY from testPersonModel";
		tsRes = ((SelectResultTimeSeries) factory.evaluateQuery(q(query), null))
				.getTimeSeriesResult();
		ts = tsRes.getSeries("SAMPLE");
		assertEquals(2, ts.size());
		assertEquals(6.0, ts.getValue(0), 0.0);
		assertEquals(0.0, ts.getValue(1), 0.0);
	}

	/**
	 * Tests the mixed usage of math- and dim-aggregations.
	 */
	@Test
	public void testAggregationMathAndDim() {
		String query;
		TimeSeriesCollection tsRes;
		TimeSeries ts;

		// load the model
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModelWithDim.xml";
		m(xml);

		// fire the UTC query
		query = "select timeseries of MAX(SUM(PERSON)) AS RESNEED, COUNT(PERSON) AS INVRESNEED, COUNT(PERSON) * MAX(COUNT(PERSON)) AS CALC ON TIME.UTC.HOUR8 from testPersonModel";
		tsRes = ((SelectResultTimeSeries) factory.evaluateQuery(q(query), null))
				.getTimeSeriesResult();

		ts = tsRes.getSeries("RESNEED");
		assertEquals(6, ts.size());
		for (int i = 0; i < ts.size(); i++) {
			assertEquals(4.0, ts.getValue(i), 0.0);
		}

		ts = tsRes.getSeries("INVRESNEED");
		assertEquals(6, ts.size());
		for (int i = 0; i < ts.size(); i++) {
			if (i == 2) {
				assertEquals(6.0, ts.getValue(i), 0.0);
			} else {
				assertEquals(4.0, ts.getValue(i), 0.0);
			}
		}

		ts = tsRes.getSeries("CALC");
		assertEquals(6, ts.size());
		for (int i = 0; i < ts.size(); i++) {
			if (i == 2) {
				assertEquals(24.0, ts.getValue(i), 0.0);
			} else {
				assertEquals(16.0, ts.getValue(i), 0.0);
			}
		}
	}

	/**
	 * Tests the exception to be thrown if an invalid time-interval is selected
	 * for the query.
	 */
	@Test
	public void testOutOfBoundIntervalWithDimension() {
		String query;

		// load the model
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModelWithDim.xml";
		m(xml);

		// fire the UTC query
		query = "select timeseries of MAX(SUM(PERSON)) AS RESNEED, COUNT(PERSON) AS INVRESNEED, COUNT(PERSON) * MAX(COUNT(PERSON)) AS CALC ON TIME.UTC.HOUR8 from testPersonModel IN [01.07.2013, 31.08.2013]";

		thrown.expect(QueryEvaluationException.class);
		thrown.expectMessage("is outside of the defined time-axis");
		((SelectResultTimeSeries) factory.evaluateQuery(q(query), null))
				.getTimeSeriesResult();
	}

	/**
	 * Tests the retrieval of a time-series for an interval which is not part of
	 * the defined time-line.
	 */
	@Test
	public void testOutOfBoundInterval() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "SELECT TIMESERIES OF SUM(SCREAMS) AS SCREAMS FROM testPersonModel IN [03.01.2014, 04.01.2014)";

		// load the model
		m(xml);

		thrown.expect(QueryEvaluationException.class);
		thrown.expectMessage("is outside of the defined time-axis");
		((SelectResultTimeSeries) factory.evaluateQuery(q(query), null))
				.getTimeSeriesResult();
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
		factory.evaluateQuery(q(query), null);
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
		factory.evaluateQuery(q(query), null);
	}

	/**
	 * Tests the iteration over the time-series.
	 */
	@Test
	public void testLabelsAndIteration() {
		TimeSeries ts;

		final String xml = "/net/meisen/dissertation/impl/parser/query/testNumberModel.xml";
		final String query = "select timeseries of mode(PAX) AS PAX, mode(CREW) AS CREW, mode(PAX + CREW) AS COMPLEX_MODE, mode(PAX) + mode(CREW) AS COMBINED_MODE from testNumberModel in [01.01.2014,05.01.2014]";

		// load the model
		m(xml);

		// fire the query
		final SelectResultTimeSeries res = (SelectResultTimeSeries) factory
				.evaluateQuery(q(query), null);
		final TimeSeriesCollection tsRes = res.getTimeSeriesResult();

		// check the labels
		final String[] labels = res.getNames();
		assertEquals(tsRes.sizeOfLabels() + 1, labels.length);
		assertEquals("ID", labels[0]);
		assertEquals("01.01.2014 00:00:00,000", labels[1]);
		assertEquals("02.01.2014 00:00:00,000", labels[2]);
		assertEquals("03.01.2014 00:00:00,000", labels[3]);
		assertEquals("04.01.2014 00:00:00,000", labels[4]);
		assertEquals("05.01.2014 00:00:00,000", labels[5]);

		int counter = 0;
		for (final Object[] o : res) {
			ts = tsRes.getSeries((String) o[0]);

			assertNotNull(ts);
			assertEquals(ts.getId(), o[0]);
			assertEquals(ts.getValue(0), o[1]);
			assertEquals(ts.getValue(1), o[2]);
			assertEquals(ts.getValue(2), o[3]);
			assertEquals(ts.getValue(3), o[4]);
			assertEquals(ts.getValue(4), o[5]);

			counter++;
		}
		assertEquals(tsRes.size(), counter);
	}

	/**
	 * Checks the iteration of the transposed time-series.
	 */
	@Test
	public void testLabelsAndTransposedIteration() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testNumberModel.xml";
		final String query = "select TRANSPOSE(timeseries) of mode(PAX) AS PAX, mode(CREW) AS CREW, mode(PAX + CREW) AS COMPLEX_MODE, mode(PAX) + mode(CREW) AS COMBINED_MODE from testNumberModel in [01.01.2014,05.01.2014]";

		// load the model
		m(xml);

		// fire the query
		final SelectResultTimeSeries res = (SelectResultTimeSeries) factory
				.evaluateQuery(q(query), null);
		final TimeSeriesCollection tsRes = res.getTimeSeriesResult();
		int counter = 0;
		for (final Object[] o : res) {
			assertNotNull(o);
			counter++;

			// make sure the whole thing ends sometimes
			if (counter > tsRes.sizeOfLabels() * tsRes.size() + 1) {
				break;
			}
		}

		assertEquals(counter, tsRes.sizeOfLabels() * tsRes.size());
	}

	/**
	 * Test the selection of records by time, which does not retrieve any
	 * records.
	 */
	@Test
	public void testEmptyRecordSelectionWithinTime() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select RECORDS from testPersonModel within [05.03.2014, 05.03.2014 02:32:00]";

		// load the model
		m(xml);

		// fire the query and get the result
		final SelectResultRecords res = (SelectResultRecords) factory
				.evaluateQuery(q(query), null);
		final Bitmap records = res.getSelectedRecords();
		final int[] ids = records.getIds();

		// check the result
		assertEquals(records.toString(), 0, ids.length);
	}

	/**
	 * Test the selection of records by an identifier.
	 */
	@Test
	public void testRecordSelectionByIdentifier() {
		IQuery query;
		SelectResultRecords result;
		int[] ids;

		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		m(xml);

		query = q("select RECORDS from testPersonModel WHERE [ID] = 2");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(1, ids.length);
		assertTrue(Arrays.binarySearch(ids, 2) > -1);

		query = q("select RECORDS from testPersonModel WHERE [ID] = 4 , 0,1,  8");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(3, ids.length);
		assertTrue(Arrays.binarySearch(ids, 0) > -1);
		assertTrue(Arrays.binarySearch(ids, 4) > -1);
		assertTrue(Arrays.binarySearch(ids, 1) > -1);

		query = q("select RECORDS from testPersonModel WITHIN [03.03.2014, 03.03.2014 02:32:00] WHERE [ID] = 2");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(0, ids.length);

		query = q("select RECORDS from testPersonModel WITHIN [03.03.2014, 03.03.2014 02:32:00] WHERE [ID] = 0");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(1, ids.length);
		assertTrue(Arrays.binarySearch(ids, 0) > -1);
	}

	/**
	 * Test the selection of records by time, which retrieves some records.
	 */
	@Test
	public void testRecordSelectionWithinTime() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select RECORDS from testPersonModel within [03.03.2014, 03.03.2014 02:32:00]";

		// load the model
		m(xml);

		// fire the query and get the result
		final SelectResultRecords res = (SelectResultRecords) factory
				.evaluateQuery(q(query), null);
		final Bitmap records = res.getSelectedRecords();
		final int[] ids = records.getIds();

		// check the result
		assertEquals(4, ids.length);
		assertTrue(Arrays.binarySearch(ids, 0) > -1);
		assertTrue(Arrays.binarySearch(ids, 1) > -1);
		assertTrue(Arrays.binarySearch(ids, 4) > -1);
		assertTrue(Arrays.binarySearch(ids, 5) > -1);
	}

	/**
	 * Test the selection of records by time, which retrieves all records.
	 */
	@Test
	public void testAllRecordSelectionWithinTime() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select RECORDS from testPersonModel within [01.03.2014, 05.03.2014 02:32:00]";

		// load the model
		m(xml);

		// fire the query and get the result
		final SelectResultRecords res = (SelectResultRecords) factory
				.evaluateQuery(q(query), null);
		final Bitmap records = res.getSelectedRecords();
		final int[] ids = records.getIds();

		// check the result
		assertEquals(records.toString(), 6, ids.length);
	}

	/**
	 * Test the selection of records using startingWith by time.
	 */
	@Test
	public void testRecordSelectionStartingWithTime() {
		IQuery query;
		SelectResultRecords result;
		int[] ids;

		// load the model
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		m(xml);

		// check a startingWith-query outside the start- & end-range
		query = q("select RECORDS from testPersonModel startingWith [05.03.2014, 05.03.2014 02:32:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(0, ids.length);

		// check a startingWith-query outside the start-range
		query = q("select RECORDS from testPersonModel startingWith [01.03.2014, 03.03.2014 02:32:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(0, ids.length);

		// check a startingWith-query with an exact start
		query = q("select RECORDS from testPersonModel startingWith [03.03.2014 00:00:00, 05.03.2014 02:32:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(4, ids.length);
		assertTrue(Arrays.binarySearch(ids, 0) > -1);
		assertTrue(Arrays.binarySearch(ids, 1) > -1);
		assertTrue(Arrays.binarySearch(ids, 4) > -1);
		assertTrue(Arrays.binarySearch(ids, 5) > -1);

		// check another startingWith-query with an exact start
		query = q("select RECORDS from testPersonModel startingWith [03.03.2014 16:20:00, 05.03.2014 02:32:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(1, ids.length);
		assertTrue(Arrays.binarySearch(ids, 2) > -1);
	}

	/**
	 * Test the selection of records using endingWith by time.
	 */
	@Test
	public void testRecordSelectionFinishingWithTime() {
		IQuery query;
		SelectResultRecords result;
		int[] ids;

		// load the model
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		m(xml);

		// check a finishingWith-query outside the start- & end-range
		query = q("select RECORDS from testPersonModel finishingWith [05.03.2014, 05.03.2014 02:32:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(0, ids.length);

		// check a finishingWith-query outside the end-range
		query = q("select RECORDS from testPersonModel finishingWith [01.03.2014, 05.03.2014 02:32:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(0, ids.length);

		// check a finishingWith-query outside the end-range
		query = q("select RECORDS from testPersonModel finishingWith [01.03.2014, 04.03.2014 23:59:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(4, ids.length);
		assertTrue(Arrays.binarySearch(ids, 0) > -1);
		assertTrue(Arrays.binarySearch(ids, 3) > -1);
		assertTrue(Arrays.binarySearch(ids, 4) > -1);
		assertTrue(Arrays.binarySearch(ids, 5) > -1);
	}

	/**
	 * Test the selection of records using during by time.
	 */
	@Test
	public void testRecordSelectionDuringWithTime() {
		IQuery query;
		SelectResultRecords result;
		int[] ids;

		// load the model
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		m(xml);

		// check a during-query outside the start-range
		query = q("select RECORDS from testPersonModel during [01.03.2014, 02.03.2014 02:32:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(0, ids.length);

		// check a during-query outside the end-range
		query = q("select RECORDS from testPersonModel during [05.03.2014, 05.03.2014 02:32:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(0, ids.length);

		// check a during-query with end outside
		query = q("select RECORDS from testPersonModel during [03.03.2014 16:19:00, 03.03.2014 17:30:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(1, ids.length);
		assertTrue(Arrays.binarySearch(ids, 2) > -1);

		// check a during-query outside the end
		query = q("select RECORDS from testPersonModel during [03.03.2014 16:19:00, 05.03.2020 00:00:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(2, ids.length);
		assertTrue(Arrays.binarySearch(ids, 2) > -1);
		assertTrue(Arrays.binarySearch(ids, 3) > -1);

		// check a during-query exactly on the end
		query = q("select RECORDS from testPersonModel during [03.03.2014 16:19:00, 04.03.2014 23:59:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(1, ids.length);
		assertTrue(Arrays.binarySearch(ids, 2) > -1);

		// check a during-query with complete range
		query = q("select RECORDS from testPersonModel during (01.03.2014 16:17:20, 20.03.2014 16:30:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(6, ids.length);

		// check a during-query with edge-timewindow's range
		query = q("select RECORDS from testPersonModel during [02.03.2014 00:00:00, 05.03.2014 00:00:00)");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(6, ids.length);

		// check a during-query with edge-timewindow's range
		query = q("select RECORDS from testPersonModel during [03.03.2014 00:00:00, 05.03.2014 00:00:00)");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(2, ids.length);
		assertTrue(Arrays.binarySearch(ids, 2) > -1);
		assertTrue(Arrays.binarySearch(ids, 3) > -1);
	}

	/**
	 * Test the selection of records using before by time.
	 */
	@Test
	public void testRecordSelectionBeforeWithTime() {
		IQuery query;
		SelectResultRecords result;
		int[] ids;

		// load the model
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		m(xml);

		// check an before-query outside the start-range
		query = q("select RECORDS from testPersonModel before [01.03.2014, 02.03.2014 02:32:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(0, ids.length);

		// check an before-query outside the end-range
		query = q("select RECORDS from testPersonModel before [05.03.2014, 05.03.2014 02:32:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(6, ids.length);

		// check an before-query
		query = q("select RECORDS from testPersonModel before (03.03.2014 16:19:00, 03.03.2014 16:30:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(1, ids.length);
		assertTrue(Arrays.binarySearch(ids, 1) > -1);

		// check an before-query
		query = q("select RECORDS from testPersonModel before (03.03.2014 17:22:00, 30.03.2014 16:30:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(2, ids.length);
		assertTrue(Arrays.binarySearch(ids, 1) > -1);
		assertTrue(Arrays.binarySearch(ids, 2) > -1);
	}

	/**
	 * Test the selection of records using containing by time.
	 */
	@Test
	public void testRecordSelectionContainingWithTime() {
		IQuery query;
		SelectResultRecords result;
		int[] ids;

		// load the model
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		m(xml);

		// check an containing-query outside the start-range
		query = q("select RECORDS from testPersonModel containing [01.03.2014, 02.03.2014 02:32:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(0, ids.length);

		// check an containing-query outside the end-range
		query = q("select RECORDS from testPersonModel Containing [05.03.2014, 05.03.2014 02:32:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(0, ids.length);

		// check an containing-query selecting some values
		query = q("select RECORDS from testPersonModel Containing [03.03.2014 16:19:00, 04.03.2014 23:59:00)");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(3, ids.length);
		assertTrue(Arrays.binarySearch(ids, 0) > -1);
		assertTrue(Arrays.binarySearch(ids, 4) > -1);
		assertTrue(Arrays.binarySearch(ids, 5) > -1);

		// check an containing-query on the edge of an interval
		query = q("select RECORDS from testPersonModel Containing (03.03.2014 17:22:00, 03.03.2014 17:23:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(4, ids.length);
		assertTrue(Arrays.binarySearch(ids, 0) > -1);
		assertTrue(Arrays.binarySearch(ids, 3) > -1);
		assertTrue(Arrays.binarySearch(ids, 4) > -1);
		assertTrue(Arrays.binarySearch(ids, 5) > -1);
	}

	/**
	 * Test the selection of records using overlapping by time.
	 */
	@Test
	public void testRecordSelectionOverlappingWithTime() {
		IQuery query;
		SelectResultRecords result;
		int[] ids;

		// load the model
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		m(xml);

		// check an overlapping-query outside the start- and end-range
		query = q("select RECORDS from testPersonModel overlapping [01.01.2014, 24.12.2014)");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(0, ids.length);

		// check an overlapping-query using the whole range
		query = q("select RECORDS from testPersonModel overlapping [01.03.2014, 05.03.2014)");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(0, ids.length);

		// check an overlapping-query at the edge at the start
		query = q("select RECORDS from testPersonModel overlapping [01.03.2014, 01.03.2014]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(0, ids.length);

		// check an overlapping-query at the edge at the end
		query = q("select RECORDS from testPersonModel overlapping [04.03.2014 23:59:00, 05.03.2014)");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(0, ids.length);

		// check an overlapping-query with just one point
		query = q("select RECORDS from testPersonModel overlapping [04.03.2014 23:58:00, 04.03.2014 23:58:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(0, ids.length);

		// overlapping at start and end
		query = q("select RECORDS from testPersonModel overlapping [03.03.2014 16:15:00, 05.03.2014]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(4, ids.length);
		assertTrue(Arrays.binarySearch(ids, 0) > -1);
		assertTrue(Arrays.binarySearch(ids, 1) > -1);
		assertTrue(Arrays.binarySearch(ids, 4) > -1);
		assertTrue(Arrays.binarySearch(ids, 5) > -1);

		// overlapping at the end of the time-window
		query = q("select RECORDS from testPersonModel overlapping [01.03.2014, 03.03.2014 17:10:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(4, ids.length);
		assertTrue(Arrays.binarySearch(ids, 0) > -1);
		assertTrue(Arrays.binarySearch(ids, 2) > -1);
		assertTrue(Arrays.binarySearch(ids, 4) > -1);
		assertTrue(Arrays.binarySearch(ids, 5) > -1);

		// overlapping at the start of the time-window
		query = q("select RECORDS from testPersonModel overlapping [03.03.2014 16:15:00, 03.03.2014 17:10:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(2, ids.length);
		assertTrue(Arrays.binarySearch(ids, 1) > -1);
		assertTrue(Arrays.binarySearch(ids, 2) > -1);
	}

	/**
	 * Test the selection of records using meeting by time.
	 */
	@Test
	public void testRecordSelectionMeetingWithTime() {
		IQuery query;
		SelectResultRecords result;
		int[] ids;

		// load the model
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		m(xml);

		// checking meeting the start-edge of time-line
		query = q("select RECORDS from testPersonModel meeting [03.03.2014 00:00:00, 03.03.2014 00:00:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(0, ids.length);

		// checking meeting the end-edge of time-line
		query = q("select RECORDS from testPersonModel meeting [04.03.2014 23:59:00, 04.03.2014 23:59:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(0, ids.length);

		// checking meeting one interval
		query = q("select RECORDS from testPersonModel meeting [03.03.2014 17:20:00, 03.03.2014 17:21:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(1, ids.length);
		assertTrue(Arrays.binarySearch(ids, 3) > -1);

		// checking meeting specific points
		query = q("select RECORDS from testPersonModel meeting [03.03.2014 16:20:00, 03.03.2014 17:21:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(2, ids.length);
		assertTrue(Arrays.binarySearch(ids, 1) > -1);
		assertTrue(Arrays.binarySearch(ids, 3) > -1);
	}

	/**
	 * Test the selection of records using after by time.
	 */
	@Test
	public void testRecordSelectionAfterWithTime() {
		IQuery query;
		SelectResultRecords result;
		int[] ids;

		// load the model
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		m(xml);

		// check an after-query outside the start-range
		query = q("select RECORDS from testPersonModel after [01.03.2014, 02.03.2014 02:32:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(6, ids.length);

		// check an after-query outside the end-range
		query = q("select RECORDS from testPersonModel after [05.03.2014, 05.03.2014 02:32:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(0, ids.length);

		// check an after-query
		query = q("select RECORDS from testPersonModel after [01.03.2014, 03.03.2014 16:20:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(1, ids.length);
		assertTrue(Arrays.binarySearch(ids, 3) > -1);

		// check an after-query
		query = q("select RECORDS from testPersonModel after [01.03.2014, 03.03.2014 16:19:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(2, ids.length);
		assertTrue(Arrays.binarySearch(ids, 2) > -1);
		assertTrue(Arrays.binarySearch(ids, 3) > -1);
	}

	/**
	 * Test the selection of records using equalTo by time, which retrieves all
	 * records.
	 */
	@Test
	public void testRecordSelectionEqualToTime() {
		IQuery query;
		SelectResultRecords result;
		int[] ids;

		// load the model
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		m(xml);

		// check an equalTo-query outside the end-range
		query = q("select RECORDS from testPersonModel equalTo [05.03.2014, 05.03.2014 02:32:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(0, ids.length);

		// check an equalTo-query outside the start-range
		query = q("select RECORDS from testPersonModel equalTo [01.01.2014, 20.01.2014 08:07:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(0, ids.length);

		// check an equalTo-query selecting one value
		query = q("select RECORDS from testPersonModel equalTo [03.03.2014 00:00:00, 03.03.2014 16:19:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(1, ids.length);
		assertTrue(Arrays.binarySearch(ids, 1) > -1);

		query = q("select RECORDS from testPersonModel equalTo [03.03.2014 00:00:00, 03.03.2014 16:20:00)");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(1, ids.length);
		assertTrue(Arrays.binarySearch(ids, 1) > -1);

		query = q("select RECORDS from testPersonModel equalTo (03.03.2014 16:19:00, 03.03.2014 17:22:00)");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(1, ids.length);
		assertTrue(Arrays.binarySearch(ids, 2) > -1);

		query = q("select RECORDS from testPersonModel equalTo (03.03.2014 16:19:00, 03.03.2014 17:22:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(0, ids.length);

		// check an eqaulTo-query with exceeded end
		query = q("select RECORDS from testPersonModel equalTo [01.03.2014 00:00:00, 28.03.2014 16:20:00)");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(0, ids.length);

		// check an eqaulTo-query bound by the time-line
		query = q("select RECORDS from testPersonModel equalTo [03.03.2014 00:00:00, 04.03.2014 23:59:00]");
		result = (SelectResultRecords) factory.evaluateQuery(query, null);
		ids = result.getSelectedRecords().getIds();
		assertEquals(3, ids.length);
		assertTrue(Arrays.binarySearch(ids, 0) > -1);
		assertTrue(Arrays.binarySearch(ids, 4) > -1);
		assertTrue(Arrays.binarySearch(ids, 5) > -1);
	}

	/**
	 * Test the selection of records by a filter, which retrieves no records.
	 */
	@Test
	public void testEmptyRecordSelectionByFilter() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select RECORDS from testPersonModel filter by PERSON='notavailable'";

		// load the model
		m(xml);

		// fire the query and get the result
		final SelectResultRecords res = (SelectResultRecords) factory
				.evaluateQuery(q(query), null);
		final Bitmap records = res.getSelectedRecords();
		final int[] ids = records.getIds();

		// check the result
		assertEquals(records.toString(), 0, ids.length);
	}

	/**
	 * Test the selection of records by a filter, which retrieves some records.
	 */
	@Test
	public void testRecordSelectionByFilter() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select RECORDS from testPersonModel filter by PERSON='Philipp'";

		// load the model
		m(xml);

		// fire the query and get the result
		final SelectResultRecords res = (SelectResultRecords) factory
				.evaluateQuery(q(query), null);
		final Bitmap records = res.getSelectedRecords();
		final int[] ids = records.getIds();

		// check the result
		assertEquals(records.toString(), 3, ids.length);
		assertTrue(Arrays.binarySearch(ids, 1) > -1);
		assertTrue(Arrays.binarySearch(ids, 2) > -1);
		assertTrue(Arrays.binarySearch(ids, 3) > -1);
	}

	/**
	 * Tests the selection of dimensional filters when selecting records.
	 */
	@Test
	public void testRecordSelectionByDimensionalFilter() {
		String query;
		SelectResultRecords res;
		Bitmap records;
		int[] ids;

		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModelWithDim.xml";

		// load the model
		m(xml);

		// fire the query and get the result
		query = "select RECORDS from testPersonModel where LOCATION.GEO.CONTINENT='EUROPE'";
		res = (SelectResultRecords) factory.evaluateQuery(q(query), null);
		records = res.getSelectedRecords();
		ids = records.getIds();

		// check the result
		assertEquals(records.toString(), 5, ids.length);
		assertTrue(Arrays.binarySearch(ids, 0) > -1);
		assertTrue(Arrays.binarySearch(ids, 1) > -1);
		assertTrue(Arrays.binarySearch(ids, 3) > -1);
		assertTrue(Arrays.binarySearch(ids, 4) > -1);
		assertTrue(Arrays.binarySearch(ids, 5) > -1);

		// fire the query and get the result
		query = "select RECORDS from testPersonModel where PERSON.GENDER.GENDER='FEMALE'";
		res = (SelectResultRecords) factory.evaluateQuery(q(query), null);
		records = res.getSelectedRecords();
		ids = records.getIds();

		// check the result
		assertEquals(records.toString(), 1, ids.length);
		assertTrue(Arrays.binarySearch(ids, 4) > -1);

		// fire the query and get the result
		query = "select RECORDS from testPersonModel where LOCATION.GEO.CONTINENT='*'";
		res = (SelectResultRecords) factory.evaluateQuery(q(query), null);
		records = res.getSelectedRecords();
		ids = records.getIds();

		// check the result
		assertEquals(records.toString(), 6, ids.length);
		assertTrue(Arrays.binarySearch(ids, 0) > -1);
		assertTrue(Arrays.binarySearch(ids, 1) > -1);
		assertTrue(Arrays.binarySearch(ids, 2) > -1);
		assertTrue(Arrays.binarySearch(ids, 3) > -1);
		assertTrue(Arrays.binarySearch(ids, 4) > -1);
		assertTrue(Arrays.binarySearch(ids, 5) > -1);

		// fire the query and get the result
		query = "select RECORDS from testPersonModel where PERSON.GENDER.GENDER='FEMALE' OR (PERSON='Philipp' AND NOT LOCATION='Aachen')";
		res = (SelectResultRecords) factory.evaluateQuery(q(query), null);
		records = res.getSelectedRecords();
		ids = records.getIds();

		// check the result
		assertEquals(records.toString(), 3, ids.length);
		assertTrue(Arrays.binarySearch(ids, 1) > -1);
		assertTrue(Arrays.binarySearch(ids, 2) > -1);
		assertTrue(Arrays.binarySearch(ids, 4) > -1);

		// fire the query and get the result
		query = "select RECORDS from testPersonModel where PERSON.GENDER.GENDER='MALE' AND PERSON='Tobias'";
		res = (SelectResultRecords) factory.evaluateQuery(q(query), null);
		records = res.getSelectedRecords();
		ids = records.getIds();

		// check the result
		assertEquals(records.toString(), 1, ids.length);
		assertTrue(Arrays.binarySearch(ids, 0) > -1);
	}

	/**
	 * Checks the usage of the limit operator.
	 */
	@Test
	public void testRecordSelectionWithLimits() {
		String query;
		SelectResultRecords res;

		// model with 6 records
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";

		// load the model
		m(xml);

		// use a filter and see if it's applied, there are 3 philipp records
		query = "select RECORDS from testPersonModel where PERSON='Philipp' limit 2, 5";
		res = (SelectResultRecords) factory.evaluateQuery(q(query), null);
		assertEquals(1, res.count());
		assertRecords(res.iterator(), new int[] { 3 });

		query = "select RECORDS from testPersonModel where PERSON='Philipp' limit 1, 2";
		res = (SelectResultRecords) factory.evaluateQuery(q(query), null);
		assertEquals(2, res.count());
		assertRecords(res.iterator(), new int[] { 2, 3 });

		query = "select RECORDS from testPersonModel where PERSON='Philipp' limit 2, 5";
		res = (SelectResultRecords) factory.evaluateQuery(q(query), null);
		assertEquals(1, res.count());
		assertRecords(res.iterator(), new int[] { 3 });

		query = "select RECORDS from testPersonModel where PERSON='Philipp' limit 4,1";
		res = (SelectResultRecords) factory.evaluateQuery(q(query), null);
		assertEquals(0, res.count());
		assertRecords(res.iterator(), new int[] {});

		query = "select RECORDS from testPersonModel where PERSON='Philipp' limit 3, 1";
		res = (SelectResultRecords) factory.evaluateQuery(q(query), null);
		assertEquals(0, res.count());
		assertRecords(res.iterator(), new int[] {});

		query = "select RECORDS from testPersonModel where PERSON='Philipp' limit 1, 1";
		res = (SelectResultRecords) factory.evaluateQuery(q(query), null);
		assertEquals(1, res.count());
		assertRecords(res.iterator(), new int[] { 2 });

		query = "select RECORDS from testPersonModel where PERSON='Philipp' limit 0";
		res = (SelectResultRecords) factory.evaluateQuery(q(query), null);
		assertEquals(3, res.count());
		assertRecords(res.iterator(), new int[] { 1, 2, 3 });

		query = "select RECORDS from testPersonModel where PERSON='Philipp' limit 2";
		res = (SelectResultRecords) factory.evaluateQuery(q(query), null);
		assertEquals(1, res.count());
		assertRecords(res.iterator(), new int[] { 3 });

		query = "select RECORDS from testPersonModel where PERSON='Philipp' limit 3";
		res = (SelectResultRecords) factory.evaluateQuery(q(query), null);
		assertEquals(0, res.count());
		assertRecords(res.iterator(), new int[] {});

		query = "select RECORDS from testPersonModel where PERSON='Philipp' limit 4";
		res = (SelectResultRecords) factory.evaluateQuery(q(query), null);
		assertEquals(0, res.count());
		assertRecords(res.iterator(), new int[] {});

		query = "select RECORDS from testPersonModel where LOCATION='Aachen' limit 0, 2";
		res = (SelectResultRecords) factory.evaluateQuery(q(query), null);
		assertEquals(2, res.count());
		assertRecords(res.iterator(), new int[] { 0, 3 });

		query = "select RECORDS from testPersonModel where LOCATION='Aachen' limit 3, 2";
		res = (SelectResultRecords) factory.evaluateQuery(q(query), null);
		assertEquals(1, res.count());
		assertRecords(res.iterator(), new int[] { 5 });
	}

	/**
	 * Helper method to validate the records respecting the {@code expected}
	 * identifiers.
	 * 
	 * @param it
	 *            the iterator to check
	 * @param expected
	 *            the expected identifiers
	 */
	protected void assertRecords(final Iterator<Object[]> it,
			final int[] expected) {

		int counter = 0;
		while (it.hasNext()) {
			final Object[] rec = it.next();
			assertTrue(rec[0] instanceof Integer);

			final int id = Numbers.castToInt((Number) rec[0]);
			assertTrue("Not found " + id + " in (" + Arrays.toString(expected)
					+ ")", Arrays.binarySearch(expected, id) > -1);

			counter++;
		}
		assertEquals(expected.length, counter);
	}

	/**
	 * Tests the usage of dimensional filters while retrieving time-series.
	 */
	@Test
	public void testTimeSeriesSelectionByDimensionalFilter() {
		String query;
		SelectResultTimeSeries res;
		TimeSeries ts;

		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModelWithDim.xml";

		// load the model
		m(xml);

		// fire the query and get the result
		query = "select TIMESERIES of count(PERSON) as CNT from testPersonModel IN [03.03.2014 16:19:00, 03.03.2014 16:20:00] where LOCATION.GEO.CONTINENT='UNKNOWN' OR PERSON='Tobias'";
		res = (SelectResultTimeSeries) factory.evaluateQuery(q(query), null);
		ts = res.getTimeSeriesResult().getSeries("CNT");
		assertEquals(2, ts.size());
		assertEquals(1.0, ts.getValue(0), 0.0);
		assertEquals(2.0, ts.getValue(1), 0.0);
	}

	/**
	 * Test the selection of records by a filter, which retrieves some records.
	 */
	@Test
	public void testAllSelectionByFilter() {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select RECORDS from testPersonModel filter by PERSON='Philipp' OR LOCATION='Aachen'";

		// load the model
		m(xml);

		// fire the query and get the result
		final SelectResultRecords res = (SelectResultRecords) factory
				.evaluateQuery(q(query), null);
		final Bitmap records = res.getSelectedRecords();
		final int[] ids = records.getIds();

		// check the result
		assertEquals(records.toString(), 6, ids.length);
	}

	/**
	 * Tests the retrieval of a record using a {@code Cache} supporting record
	 * holding.
	 * 
	 * @throws ParseException
	 *             if a date cannot be parsed
	 */
	@Test
	public void testRecordRetrieval() throws ParseException {
		System.setProperty("test.rndUuid", UUID.randomUUID().toString());

		// cleanUp temp of previous tests
		Files.deleteOnExitDir(new File(System.getProperty("java.io.tmpdir")),
				"^tmpSelectQueriesRecordRetrieval\\-.*$");

		final String xml = "/net/meisen/dissertation/impl/parser/query/testRecordRetrieval.xml";
		final String query = "select RECORDS from selectQueriesRecordRetrieval filter by PERSON='Philipp' AND LOCATION='Aachen'";

		// load the model
		m(xml);

		// fire the query and get the result
		final SelectResultRecords res = (SelectResultRecords) factory
				.evaluateQuery(q(query), null);

		/*
		 * Check the names!
		 */
		assertNotNull(res.getNames());
		final List<String> names = Arrays.asList(res.getNames());
		assertEquals(6, names.size());

		// check the expected names to be found
		assertTrue(names.contains("[ID]"));
		assertTrue(names.contains("[START]"));
		assertTrue(names.contains("[END]"));
		assertTrue(names.contains("PERSON"));
		assertTrue(names.contains("LOCATION"));
		assertTrue(names.contains("SCREAMS"));

		// determine the position if the values
		final int posId = 0;
		final int posStart = 1;
		final int posEnd = 2;
		final int posPerson = Collections.getPosition(names, "PERSON");
		final int posLocation = Collections.getPosition(names, "LOCATION");
		final int posScreams = Collections.getPosition(names, "SCREAMS");

		/*
		 * Check the types!
		 */
		final Class<?>[] types = res.getTypes();
		assertNotNull(types);
		assertEquals(6, types.length);
		assertEquals(Integer.class, types[posId]);
		assertEquals(Date.class, types[posStart]);
		assertEquals(Date.class, types[posEnd]);
		assertEquals(String.class, types[posPerson]);
		assertEquals(String.class, types[posLocation]);
		assertEquals(Integer.class, types[posScreams]);

		// check the values of the record
		final Iterator<Object[]> it = res.iterator();
		int counter = 0;
		while (it.hasNext()) {
			final Object[] rec = it.next();

			assertEquals(3, rec[posId]);
			assertEquals(Dates.parseDate("03.03.2014 17:22:00",
					"dd.MM.yyyy HH:mm:ss"), rec[posStart]);
			assertEquals(Dates.parseDate("04.03.2014 23:59:00",
					"dd.MM.yyyy HH:mm:ss"), rec[posEnd]);
			assertEquals("Philipp", rec[posPerson]);
			assertEquals("Aachen", rec[posLocation]);
			assertEquals(0, rec[posScreams]);

			counter++;
		}
		assertEquals(counter, 1);

		// cleanUp as much as possible of this test
		Files.deleteOnExitDir(new File(System.getProperty("java.io.tmpdir")),
				"^tmpSelectQueriesRecordRetrieval\\-.*$");
	}

	/**
	 * Checks the retrieval of records only.
	 */
	@Test
	public void testRecordIdRetrieval() {
		System.setProperty("test.rndUuid", UUID.randomUUID().toString());

		// cleanUp temp of previous tests
		Files.deleteOnExitDir(new File(System.getProperty("java.io.tmpdir")),
				"^tmpSelectQueriesRecordRetrieval\\-.*$");

		final String xml = "/net/meisen/dissertation/impl/parser/query/testRecordRetrieval.xml";
		final String query = "select IDS(RECORDS) from selectQueriesRecordRetrieval filter by PERSON='Tobias' OR LOCATION='Mönchengladbach'";

		// load the model
		m(xml);

		// fire the query and get the result
		final SelectResultRecords res = (SelectResultRecords) factory
				.evaluateQuery(q(query), null);

		/*
		 * Check the names!
		 */
		assertNotNull(res.getNames());
		final List<String> names = Arrays.asList(res.getNames());
		assertEquals(1, names.size());

		// check the expected names to be found
		assertTrue(names.contains("[ID]"));

		/*
		 * Check the types!
		 */
		final Class<?>[] types = res.getTypes();
		assertNotNull(types);
		assertEquals(1, types.length);
		assertEquals(Integer.class, types[0]);

		// check the values of the record
		final Iterator<Object[]> it = res.iterator();
		int counter = 0;
		while (it.hasNext()) {
			final Object[] rec = it.next();

			assertEquals(counter, rec[0]);
			counter++;
		}
		assertEquals(counter, 2);

		// cleanUp as much as possible of this test
		Files.deleteOnExitDir(new File(System.getProperty("java.io.tmpdir")),
				"^tmpSelectQueriesRecordRetrieval\\-.*$");
	}

	/**
	 * Checks the retrieval of count.
	 */
	@Test
	public void testRecordCount() {
		System.setProperty("test.rndUuid", UUID.randomUUID().toString());

		// cleanUp temp of previous tests
		Files.deleteOnExitDir(new File(System.getProperty("java.io.tmpdir")),
				"^tmpSelectQueriesRecordRetrieval\\-.*$");

		final String xml = "/net/meisen/dissertation/impl/parser/query/testRecordRetrieval.xml";
		final String query = "select COUNT(RECORDS) from selectQueriesRecordRetrieval startingwith [03.03.2014 00:00:00, 03.03.2014 00:00:00] filter by LOCATION='Aachen'";

		// load the model
		m(xml);

		// fire the query and get the result
		final SelectResultRecords res = (SelectResultRecords) factory
				.evaluateQuery(q(query), null);

		/*
		 * Check the names!
		 */
		assertNotNull(res.getNames());
		final List<String> names = Arrays.asList(res.getNames());
		assertEquals(1, names.size());

		// check the expected names to be found
		assertTrue(names.contains("[COUNT]"));

		/*
		 * Check the types!
		 */
		final Class<?>[] types = res.getTypes();
		assertNotNull(types);
		assertEquals(1, types.length);
		assertEquals(Integer.class, types[0]);

		// check the values of the record
		final Iterator<Object[]> it = res.iterator();
		int counter = 0;
		while (it.hasNext()) {
			final Object[] rec = it.next();
			assertEquals(3, rec[0]);
			counter++;
		}
		assertEquals(counter, 1);

		// cleanUp as much as possible of this test
		Files.deleteOnExitDir(new File(System.getProperty("java.io.tmpdir")),
				"^tmpSelectQueriesRecordRetrieval\\-.*$");
	}

	/**
	 * Tests the retrieval of records based on the identifiers.
	 * 
	 * @throws ParseException
	 *             if a date could not be parsed
	 */
	@Test
	public void testRecordsRetrievalFromIds() throws ParseException {
		final String xml = "/net/meisen/dissertation/impl/parser/query/testPersonModel.xml";
		final String query = "select RECORDS from testPersonModel within [01.03.2014, 05.03.2014 02:32:00]";

		// load the model
		m(xml);

		// fire the query and get the result
		final SelectResultRecords res = (SelectResultRecords) factory
				.evaluateQuery(q(query), null);

		Iterator<Object[]> it = res.iterator();
		while (it.hasNext()) {
			final Object[] rec = it.next();

			switch ((Integer) rec[0]) {
			case 0:
				assertEquals(Dates.parseDate("03.03.2014 00:00:00",
						"dd.MM.yyyy HH:m:ss"), rec[1]);
				assertEquals(Dates.parseDate("04.03.2014 23:59:00",
						"dd.MM.yyyy HH:m:ss"), rec[2]);
				assertEquals(0, rec[3]);
				assertEquals("Aachen", rec[4]);
				assertEquals("Tobias", rec[5]);
				break;
			case 1:
				assertEquals(Dates.parseDate("03.03.2014 00:00:00",
						"dd.MM.yyyy HH:m:ss"), rec[1]);
				assertEquals(Dates.parseDate("03.03.2014 16:19:00",
						"dd.MM.yyyy HH:m:ss"), rec[2]);
				assertEquals(3, rec[3]);
				assertEquals("Mönchengladbach", rec[4]);
				assertEquals("Philipp", rec[5]);
				break;
			case 2:
				assertEquals(Dates.parseDate("03.03.2014 16:20:00",
						"dd.MM.yyyy HH:m:ss"), rec[1]);
				assertEquals(Dates.parseDate("03.03.2014 17:21:00",
						"dd.MM.yyyy HH:m:ss"), rec[2]);
				assertEquals(0, rec[3]);
				assertEquals("Undefined", rec[4]);
				assertEquals("Philipp", rec[5]);
				break;
			case 3:
				assertEquals(Dates.parseDate("03.03.2014 17:22:00",
						"dd.MM.yyyy HH:m:ss"), rec[1]);
				assertEquals(Dates.parseDate("04.03.2014 23:59:00",
						"dd.MM.yyyy HH:m:ss"), rec[2]);
				assertEquals(0, rec[3]);
				assertEquals("Aachen", rec[4]);
				assertEquals("Philipp", rec[5]);
				break;
			case 4:
				assertEquals(Dates.parseDate("03.03.2014 00:00:00",
						"dd.MM.yyyy HH:m:ss"), rec[1]);
				assertEquals(Dates.parseDate("04.03.2014 23:59:00",
						"dd.MM.yyyy HH:m:ss"), rec[2]);
				assertEquals(0, rec[3]);
				assertEquals("Aachen", rec[4]);
				assertEquals("Debbie", rec[5]);
				break;
			case 5:
				assertEquals(Dates.parseDate("03.03.2014 00:00:00",
						"dd.MM.yyyy HH:m:ss"), rec[1]);
				assertEquals(Dates.parseDate("04.03.2014 23:59:00",
						"dd.MM.yyyy HH:m:ss"), rec[2]);
				assertEquals(12, rec[3]);
				assertEquals("Aachen", rec[4]);
				assertEquals("Edison", rec[5]);
				break;
			default:
				fail("Invalid amount of records: " + Arrays.asList(rec));
				break;
			}
		}
	}
}
