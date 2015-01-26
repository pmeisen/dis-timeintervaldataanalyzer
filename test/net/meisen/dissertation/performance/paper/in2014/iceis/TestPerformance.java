package net.meisen.dissertation.performance.paper.in2014.iceis;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.sql.SQLException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.Db;
import net.meisen.dissertation.help.ModuleAndDbBasedTest;
import net.meisen.dissertation.help.Performance;
import net.meisen.dissertation.impl.datasets.SingleStaticDataSet;
import net.meisen.dissertation.impl.measures.Count;
import net.meisen.dissertation.impl.measures.Max;
import net.meisen.dissertation.impl.measures.Min;
import net.meisen.dissertation.impl.measures.Sum;
import net.meisen.dissertation.impl.parser.query.DimensionSelector;
import net.meisen.dissertation.impl.parser.query.select.DescriptorComperator;
import net.meisen.dissertation.impl.parser.query.select.DimensionComperator;
import net.meisen.dissertation.impl.parser.query.select.IComperator;
import net.meisen.dissertation.impl.parser.query.select.IntervalRelation;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.parser.query.select.SelectResultRecords;
import net.meisen.dissertation.impl.parser.query.select.SelectResultTimeSeries;
import net.meisen.dissertation.impl.parser.query.select.SelectResultType;
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
import net.meisen.dissertation.model.data.DimensionModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.dissertation.model.dimensions.TimeMemberRange;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.measures.IAggregationFunction;
import net.meisen.dissertation.model.parser.query.IQueryFactory;
import net.meisen.dissertation.model.parser.query.IQueryResult;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.dissertation.performance.paper.in2014.iceis.naive.IntervalTree;
import net.meisen.dissertation.performance.paper.in2014.iceis.naive.IntervalTree.IntervalData;
import net.meisen.general.genmisc.types.Numbers;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * The performance tests used for the ITNG paper 2015: {@code Bitmap-Based
 * On-Line Analytical Processing of Time Interval Data}
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TidaConfig.class)
@ContextFile("sbconfigurator-core.xml")
public class TestPerformance extends ModuleAndDbBasedTest {
	private static int RUNS = 100;
	private static int INITRUNS = 5;

	@Autowired
	private TidaModelHandler loader;

	@Autowired
	@Qualifier(DefaultValues.QUERYFACTORY_ID)
	private IQueryFactory queryFactory;

	/**
	 * Tests the TIDA performance.
	 * 
	 * @throws IOException
	 *             if the model cannot be loaded
	 * @throws SQLException
	 * @throws ParseException
	 */
	@Test
	public void testPerformance() throws IOException, SQLException,
			ParseException {
		testSizedPerformance(10);
		testSizedPerformance(100);
		testSizedPerformance(1000);
		testSizedPerformance(10000);
		testSizedPerformance(100000);
		testSizedPerformance(1000000);
	}

	/**
	 * Tests the performance for the specified size.
	 * 
	 * @param limit
	 *            the limit (i.e. amount of data)
	 * @throws IOException
	 *             if a file cannot be read
	 * @throws SQLException
	 *             if the query cannot be fired to retrieve the data
	 * @throws ParseException
	 *             if a date cannot be parsed
	 */
	protected void testSizedPerformance(final int limit) throws IOException,
			SQLException, ParseException {
		System.out.println("Starting with LIMIT: " + limit);

		final Db db = getDb("tida",
				"/net/meisen/dissertation/performance/paper/in2014/iceis/data/ghdataHsql.zip");

		// Sleep a little
		/*
		 * System.out.println("SLEEP"); try { Thread.sleep(1000000); } catch
		 * (final InterruptedException e) { }
		 */

		final TidaModel model = loader
				.loadViaXslt("/net/meisen/dissertation/performance/paper/in2014/iceis/model/tida-model-minute.xml");
		final BaseMapper<?> mapper = model.getIntervalModel()
				.getTimelineMapper();

		// get the data
		final String query = "SELECT KEY, PERSON, TASKTYPE, WORKAREA, INTERVAL_START, INTERVAL_END FROM SMC_DATA ORDER BY RAND()";
		// final String query =
		// "SELECT KEY, PERSON, TASKTYPE, WORKAREA, INTERVAL_START, INTERVAL_END FROM SMC_DATA";
		final List<Map<String, Object>> rows = db.query("tida", query
				+ " LIMIT " + limit);
		// Print the retrieved records
		// for (final Map<String, Object> rec : rows) {
		// System.out.println(rec);
		// }

		// add the data to the model, the interval-tree and the map is naive one
		final List<IntervalData<Integer>> list = new ArrayList<IntervalData<Integer>>();
		model.setBulkLoad(true);
		try {
			int i = 0;
			final Iterator<Map<String, Object>> it = rows.iterator();
			while (it.hasNext()) {
				final Map<String, Object> row = it.next();
				final Object rStart = row.get("INTERVAL_START");
				final Object rEnd = row.get("INTERVAL_END");

				if (mapper.isSmallerThanStart(rEnd)
						|| mapper.isLargerThanEnd(rStart)) {
					it.remove();
					continue;
				} else {
					final long start = mapper.mapToLong(rStart);
					final long end = mapper.mapToLong(rEnd);

					list.add(new IntervalData<Integer>(start, end, i));

					final SingleStaticDataSet dataSet = new SingleStaticDataSet(
							row);
					model.loadRecord(dataSet);

					i++;
				}
			}
		} finally {
			model.setBulkLoad(false);
		}
		final IntervalTree<Integer> iTree = new IntervalTree<Integer>(list);
		doFirstQuery(model, iTree, rows);
		doSecondQuery(model, iTree, rows);
		doThirdQuery(model, iTree, rows);
		doFourthQuery(model, iTree, rows);
		doFifthQuery(model, iTree, rows);

		// cleanUp
		loader.unloadAll();
		db.shutDownDb();
	}

	private void doFirstQuery(TidaModel model, IntervalTree<Integer> iTree,
			List<Map<String, Object>> rows) {

		/*
		 * 1. Test: OF COUNT(TASKTYPE) IN [01.JAN, 01.FEB) WHERE
		 * WA.LOC.TYPE='Gate'
		 */
		final String statement = "SELECT TIMESERIES OF COUNT(TASKTYPE) FROM tidaModel IN [01.01.2008 00:00:00, 31.01.2008 23:59:00] WHERE WA.LOC.TYPE='Gate'";

		doQuery(model, iTree, rows, statement);
	}

	private void doSecondQuery(final TidaModel model,
			final IntervalTree<Integer> iTree,
			final List<Map<String, Object>> rows) {

		/*
		 * 2. Test: OF SUM(TASKTYPE) ON TIME.DEF.DAY IN [01.JAN, 01.FEB) WHERE
		 * WORKAREA='SEN.W14'
		 */
		final String statement = "SELECT TIMESERIES OF SUM(TASKTYPE) ON TIME.DEF.DAY FROM tidaModel IN [01.01.2008 00:00:00, 31.01.2008 23:59:00] WHERE WORKAREA='SEN.W14'";

		doQuery(model, iTree, rows, statement);
	}

	private void doThirdQuery(final TidaModel model,
			final IntervalTree<Integer> iTree,
			final List<Map<String, Object>> rows) {

		/*
		 * 3. Test: OF AVG(COUNT(WORKAREA)) ON TIME.DEF.DAY IN [01.JAN, 01.FEB)
		 * WHERE TASKTYPE='short'
		 */
		final String statement = "SELECT TIMESERIES OF MAX(COUNT(WORKAREA)) ON TIME.DEF.DAY FROM tidaModel IN [01.01.2008 00:00:00, 31.01.2008 23:59:00] WHERE TASKTYPE='short'";

		doQuery(model, iTree, rows, statement);
	}

	private void doFourthQuery(final TidaModel model,
			final IntervalTree<Integer> iTree,
			final List<Map<String, Object>> rows) {

		/*
		 * 4. Test: OF MAX(SUM(PERSON) / COUNT(PERSON)) ON TIME.DEF.MIN5DAY IN
		 * [01.JAN, 01.FEB) WHERE TASKTYPE='long'
		 */
		final String statement = "SELECT TIMESERIES OF MAX(SUM(PERSON) / COUNT(PERSON)) ON TIME.DEF.MIN5DAY FROM tidaModel IN [01.01.2008 00:00:00, 31.01.2008 23:59:00] WHERE TASKTYPE='long'";

		doQuery(model, iTree, rows, statement);
	}

	private void doFifthQuery(final TidaModel model,
			final IntervalTree<Integer> iTree,
			final List<Map<String, Object>> rows) {

		/*
		 * 5. Test: OF MIN(TASKTYPE) ON TIME.DEF.MIN5DAY IN [01.JAN 00:00,
		 * 31.JAN 23:59] WHERE WA.LOC.TYPE='RAMP' OR PERSON='*9'
		 */
		final String statement = "SELECT TIMESERIES OF MIN(TASKTYPE) ON TIME.DEF.MIN5DAY FROM tidaModel IN [01.01.2008 00:00:00, 31.01.2008 23:59:00] WHERE WA.LOC.TYPE='Ramp' OR PERSON='*9'";

		doQuery(model, iTree, rows, statement);
	}

	private void doQuery(final TidaModel model,
			final IntervalTree<Integer> iTree,
			final List<Map<String, Object>> rows, final String statement) {
		System.out.println("QUERY: " + statement);

		final DimensionModel dimModel = model.getDimensionModel();
		final BaseMapper<?> mapper = model.getIntervalModel()
				.getTimelineMapper();
		final BaseIndexFactory factory = model.getIndexFactory();

		// parsing
		final SelectQuery query = queryFactory.parseQuery(statement);

		final TimeSeriesCollection naiveRes = measureNaive(rows, dimModel,
				mapper, factory, query);
		final TimeSeriesCollection intTreeARes = measureIntTreeA(iTree, rows,
				dimModel, mapper, factory, query);
		final TimeSeriesCollection intTreeBRes = measureIntTreeB(iTree, rows,
				dimModel, mapper, factory, query);
		final TimeSeriesCollection tidaRes = measureTidaModel(model, query);
		getCount(model, query);

		// make sure everything is equal
		assertEquals(tidaRes, naiveRes);
		assertEquals(tidaRes, intTreeARes);
		assertEquals(tidaRes, intTreeBRes);
	}

	/**
	 * Test the performance of a naive implementation.
	 * 
	 * @param data
	 *            the data
	 * @param dimModel
	 *            the defined {@code DimensionModel}
	 * @param mapper
	 *            the mapper to be used
	 * @param factory
	 *            the index-factory
	 * @param query
	 *            the parsed query
	 * 
	 * @return the result of the naive algorithm
	 */
	protected TimeSeriesCollection measureNaive(
			final List<Map<String, Object>> data,
			final DimensionModel dimModel, final BaseMapper<?> mapper,
			final BaseIndexFactory factory, final SelectQuery query) {
		final long[] results = new long[RUNS];

		// do some parsing
		Performance performance;

		// iteration
		TimeSeriesCollection tsc = null;
		for (int i = 0; i < RUNS + INITRUNS; i++) {
			performance = new Performance();
			if (i >= INITRUNS) {
				performance.start(false);
			}

			final List<Map<String, Object>> filteredRecords = new ArrayList<Map<String, Object>>();
			for (final Map<String, Object> record : data) {
				if (!checkDate(mapper, (Date) query.getInterval().getStart(),
						(Date) query.getInterval().getEnd(), record)) {
					continue;
				} else if (!checkFilter(query.getFilter(), record, null)) {
					continue;
				} else {
					filteredRecords.add(record);
				}
			}

			final IRecordsFilter filter = new IRecordsFilter() {

				@Override
				public List<Map<String, Object>> apply(final long start,
						final long end) {
					return apply(start, end, filteredRecords);
				}

				@Override
				public List<Map<String, Object>> apply(final long start,
						final long end, final List<Map<String, Object>> records) {

					final List<Map<String, Object>> tpRecords = new ArrayList<Map<String, Object>>();
					for (final Map<String, Object> record : records) {
						final long rStart = mapper.mapToLong(record
								.get("INTERVAL_START"));
						final long rEnd = mapper.mapToLong(record
								.get("INTERVAL_END"));
						if (start <= rEnd && end >= rStart) {
							tpRecords.add(record);
						}
					}

					return tpRecords;
				}

				@Override
				public boolean incSupport() {
					return true;
				}
			};
			tsc = calculateMeasures(query, dimModel, filter, mapper, factory);

			if (i >= INITRUNS) {
				results[i - INITRUNS] = performance.stop(false)[0];
			}
		}

		// result
		printResult("Naive", results);
		System.gc();

		return tsc;
	}

	/**
	 * Measure the performance of the interval-tree (A).
	 * 
	 * @param iTree
	 *            the interval tree
	 * @param data
	 *            the data
	 * @param dimModel
	 *            the defined {@code DimensionModel}
	 * @param mapper
	 *            the mapper to be used
	 * @param factory
	 *            the index-factory
	 * @param query
	 *            the parsed query
	 * 
	 * @return the result of the IntervalTree algorithm (A)
	 */
	protected TimeSeriesCollection measureIntTreeA(
			final IntervalTree<Integer> iTree,
			final List<Map<String, Object>> data,
			final DimensionModel dimModel, final BaseMapper<?> mapper,
			final BaseIndexFactory factory, final SelectQuery query) {
		final long[] results = new long[RUNS];

		// do some parsing
		Performance performance;

		// iteration
		TimeSeriesCollection tsc = null;
		for (int i = 0; i < RUNS + INITRUNS; i++) {
			performance = new Performance();
			if (i >= INITRUNS) {
				performance.start(false);
			}

			final IntervalData<Integer> res = iTree.query(
					mapper.mapToLong(query.getInterval().getStart()),
					mapper.mapToLong(query.getInterval().getEnd()));
			final Collection<Integer> positions = res == null ? Collections
					.<Integer> emptyList() : res.getData();

			final List<Map<String, Object>> filteredRecords = new ArrayList<Map<String, Object>>();
			for (final Integer position : positions) {
				final Map<String, Object> record = data.get(position);

				if (!checkFilter(query.getFilter(), record, null)) {
					continue;
				} else {
					filteredRecords.add(record);
				}
			}

			final IRecordsFilter filter = new IRecordsFilter() {

				@Override
				public List<Map<String, Object>> apply(final long start,
						final long end) {
					return apply(start, end, filteredRecords);
				}

				@Override
				public List<Map<String, Object>> apply(final long start,
						final long end, final List<Map<String, Object>> records) {

					final List<Map<String, Object>> tpRecords = new ArrayList<Map<String, Object>>();
					for (final Map<String, Object> record : records) {
						final long rStart = mapper.mapToLong(record
								.get("INTERVAL_START"));
						final long rEnd = mapper.mapToLong(record
								.get("INTERVAL_END"));
						if (start <= rEnd && end >= rStart) {
							tpRecords.add(record);
						}
					}

					return tpRecords;
				}

				@Override
				public boolean incSupport() {
					return true;
				}
			};
			tsc = calculateMeasures(query, dimModel, filter, mapper, factory);

			if (i >= INITRUNS) {
				results[i - INITRUNS] = performance.stop(false)[0];
			}
		}

		// result
		printResult("IntervalTree (A)", results);
		System.gc();

		return tsc;
	}

	/**
	 * Measure the performance of the interval-tree (A).
	 * 
	 * @param iTree
	 *            the interval tree
	 * @param data
	 *            the data
	 * @param dimModel
	 *            the defined {@code DimensionModel}
	 * @param mapper
	 *            the mapper to be used
	 * @param factory
	 *            the index-factory
	 * @param query
	 *            the parsed query
	 * 
	 * @return the result of the IntervalTree algorithm (B)
	 */
	protected TimeSeriesCollection measureIntTreeB(
			final IntervalTree<Integer> iTree,
			final List<Map<String, Object>> data,
			final DimensionModel dimModel, final BaseMapper<?> mapper,
			final BaseIndexFactory factory, final SelectQuery query) {
		final long[] results = new long[RUNS];

		// do some parsing
		Performance performance;

		// iteration
		TimeSeriesCollection tsc = null;
		for (int i = 0; i < RUNS + INITRUNS; i++) {
			performance = new Performance();
			if (i >= INITRUNS) {
				performance.start(false);
			}

			final IntervalData<Integer> res = iTree.query(
					mapper.mapToLong(query.getInterval().getStart()),
					mapper.mapToLong(query.getInterval().getEnd()));
			final Collection<Integer> positions = res == null ? Collections
					.<Integer> emptyList() : res.getData();

			final List<Map<String, Object>> filteredRecords = new ArrayList<Map<String, Object>>();
			for (final Integer position : positions) {
				final Map<String, Object> record = data.get(position);

				if (!checkFilter(query.getFilter(), record, null)) {
					continue;
				} else {
					filteredRecords.add(record);
				}
			}

			// create the new IntervalTree for the filtered values
			final List<IntervalData<Integer>> filteredList = new ArrayList<IntervalData<Integer>>();
			int counter = 0;
			for (final Map<String, Object> filteredRecord : filteredRecords) {

				// create the stuff for the interval-tree
				final long start = mapper.mapToLong(filteredRecord
						.get("INTERVAL_START"));
				final long end = mapper.mapToLong(filteredRecord
						.get("INTERVAL_END"));
				filteredList
						.add(new IntervalData<Integer>(start, end, counter));

				counter++;
			}
			final IntervalTree<Integer> filteredITree = filteredList.size() == 0 ? null
					: new IntervalTree<Integer>(filteredList);

			final IRecordsFilter filter = new IRecordsFilter() {

				@Override
				public List<Map<String, Object>> apply(final long start,
						final long end) {
					if (filteredITree == null) {
						return Collections.<Map<String, Object>> emptyList();
					} else {
						return filteredITree.query(start, end, filteredRecords);
					}
				}

				@Override
				public List<Map<String, Object>> apply(final long start,
						final long end, final List<Map<String, Object>> records) {
					if (records == null) {
						return apply(start, end);
					} else {
						throw new UnsupportedOperationException();
					}
				}

				@Override
				public boolean incSupport() {
					return false;
				}
			};
			tsc = calculateMeasures(query, dimModel, filter, mapper, factory);

			if (i >= INITRUNS) {
				results[i - INITRUNS] = performance.stop(false)[0];
			}
		}

		// result
		printResult("IntervalTree (B)", results);
		System.gc();

		return tsc;
	}

	/**
	 * Function to test the performance of the TidaModel.
	 * 
	 * @param model
	 *            the model to be tested
	 * @param query
	 *            the query to be fired
	 * 
	 * @return the result of the bitmap-based implementation
	 */
	protected TimeSeriesCollection measureTidaModel(final TidaModel model,
			final SelectQuery query) {
		final long[] results = new long[RUNS];
		Performance performance;

		// iteration
		TimeSeriesCollection tsc = null;
		for (int i = 0; i < RUNS + INITRUNS; i++) {
			performance = new Performance();
			if (i >= INITRUNS) {
				performance.start(false);
			}

			final SelectResultTimeSeries resTsc = queryFactory.evaluateQuery(
					query, null);
			tsc = resTsc.getTimeSeriesResult();

			if (i >= INITRUNS) {
				results[i - INITRUNS] = performance.stop(false)[0];
			}
		}

		// result
		printResult("TidaModel", results);
		System.gc();

		return tsc;
	}

	/**
	 * Print the results of the test.
	 * 
	 * @param type
	 *            the type of the test
	 * @param results
	 *            the array of the results
	 */
	protected void printResult(final String type, final long[] results) {
		long sum = 0;
		long min = Long.MAX_VALUE;
		long max = Long.MIN_VALUE;
		for (int l = 0; l < RUNS; l++) {
			sum += results[l];
			min = results[l] < min ? results[l] : min;
			max = results[l] > max ? results[l] : max;
		}

		System.out.println("----" + type + "----");
		System.out.println("FINAL (AVG): " + (sum / RUNS));
		System.out.println("FINAL (MIN): " + min);
		System.out.println("FINAL (MAX): " + max);
		System.out.println("--------------------");
	}

	/**
	 * Get the count of the slected records.
	 * 
	 * @param model
	 *            the model
	 * @param orgQuery
	 *            the query fired
	 */
	protected void getCount(final TidaModel model, final SelectQuery orgQuery) {
		final SelectQuery query = new SelectQuery();
		query.setInterval(orgQuery.getInterval());
		query.setCount(true);
		query.setResultType(SelectResultType.RECORDS);
		query.setModelId(orgQuery.getModelId());
		query.setIntervalRelation(IntervalRelation.WITHIN);
		query.setFilter(orgQuery.getFilter());
		for (final DescriptorMathTree measure : orgQuery.getMeasures()) {
			query.addMeasure(measure);
		}

		final IQueryResult res = queryFactory.evaluateQuery(query, null);
		if (res instanceof SelectResultRecords) {
			System.out.println("COUNT: "
					+ ((SelectResultRecords) res).getSelectedRecords()
							.determineCardinality());
		} else {
			System.out.println("COUNT: INVALID RESULT!");
		}

		// show the records
		// query.setCount(false);
		// final SelectResultRecords recRes = queryFactory.evaluateQuery(query,
		// null);
		// final java.util.Iterator<Object[]> it = recRes.iterator();
		// while (it.hasNext()) {
		// System.out.println(java.util.Arrays.asList(it.next()));
		// }

	}

	private boolean checkFilter(final DescriptorLogicTree filter,
			final Map<String, Object> record, LogicalOperatorNode node) {
		if (node == null) {
			node = filter.getRoot();
		}

		final List<Boolean> values = new ArrayList<Boolean>();
		final List<ILogicalTreeElement> children = node.getChildren();
		for (int i = children.size(); i > 0; i--) {
			final ILogicalTreeElement child = children.get(i - 1);

			if (child instanceof LogicalOperatorNode) {
				values.add(checkFilter(filter, record,
						(LogicalOperatorNode) child));
			} else if (child instanceof DescriptorLeaf) {
				final DescriptorLeaf leaf = (DescriptorLeaf) child;
				final IComperator cmp = leaf.get();

				if (cmp instanceof DescriptorComperator) {
					final DescriptorComperator dCmp = (DescriptorComperator) cmp;

					// get the value of the record
					Object value = record.get(dCmp.getId());

					// check the value
					if (value == null) {
						values.add(false);
					} else if (dCmp.containsWildchar()) {
						values.add(value.toString().matches(dCmp.getValue()));
					} else {
						values.add(value.toString().equals(dCmp.getValue()));
					}
				} else if (cmp instanceof DimensionComperator) {
					final DimensionComperator dCmp = (DimensionComperator) cmp;
					final DimensionSelector dSel = dCmp.getDimension();

					if (dSel.toString().equals("WA.LOC.TYPE")
							&& dCmp.getRawValue().equals("Gate")) {
						final Object value = record.get("WORKAREA");
						values.add(value.toString().matches("BIE\\..*"));
					} else if (dSel.toString().equals("WA.LOC.TYPE")
							&& dCmp.getRawValue().equals("Ramp")) {
						final Object value = record.get("WORKAREA");
						values.add(value.toString().matches("EDI\\..*"));
					} else {
						fail("Not avialable currently '" + dSel + "', '"
								+ dCmp.getRawValue() + "'");
					}
				} else {
					fail("Invalid compeartor");
				}
			} else {
				fail("Invalid node '" + child + "'");
			}
		}

		final LogicalOperator lo = node.get();
		if (LogicalOperator.NOT.equals(lo)) {
			if (values.size() != 1) {
				fail("Invalid size for not '" + values + "'");
			}

			// apply the not
			return !values.get(0);
		} else {
			final int size = values.size();

			if (size < 1) {
				fail("Invalid formular");
			} else if (size > 1) {
				if (LogicalOperator.AND.equals(lo)) {
					for (final Boolean val : values) {
						if (!val) {
							return false;
						}
					}
					return true;
				} else if (LogicalOperator.OR.equals(lo)) {
					for (final Boolean val : values) {
						if (val) {
							return val;
						}
					}
					return false;
				} else {
					fail("Invalid operator '" + lo + "'");
				}
			} else {
				return values.get(0);
			}
		}

		return false;
	}

	private boolean checkDate(final BaseMapper<?> m, final Date sDate,
			final Date eDate, final Map<String, Object> record) {

		final Object sIntervalDate = record.get("INTERVAL_START");
		final Object eIntervalDate = record.get("INTERVAL_END");

		final long sTw = m.mapToLong(sDate);
		final long eTw = m.mapToLong(eDate);
		final long sInterval = m.mapToLong(sIntervalDate);
		final long eInterval = m.mapToLong(eIntervalDate);

		return sTw <= eInterval && eTw >= sInterval;
	}

	private TimeSeriesCollection calculateMeasures(final SelectQuery query,
			final DimensionModel dimModel, final IRecordsFilter filter,
			final BaseMapper<?> mapper, final BaseIndexFactory factory) {
		final long s = mapper.mapToLong(query.getInterval().getStart());
		final long e = mapper.mapToLong(query.getInterval().getEnd());

		final TimeSeriesCollection tsc;
		if (query.getMeasureDimension() == null) {
			tsc = new TimeSeriesCollection(Numbers.castToInt(e - s + 1), query
					.getInterval().getStart().getClass(), factory);

			for (long i = s; i <= e; i++) {
				final List<Map<String, Object>> tpRecords = filter.apply(i, i);

				// set the label
				final Object labelValue = mapper.resolve(i);
				tsc.setLabel(Numbers.castToInt(i - s),
						mapper.format(labelValue), labelValue);

				for (final DescriptorMathTree measure : query.getMeasures()) {
					final IMathTreeElement node = ((MathOperatorNode) measure
							.getRoot()).getChild(0);
					final List<Double> res = calculateMeasure(tpRecords, node);
					assert 1 == res.size();

					// set the value within the series
					final TimeSeries ts = getTimeSeries(tsc, measure.getId());
					ts.setValue(Numbers.castToInt(i), res.get(0).doubleValue());
				}
			}
		} else {
			final Set<TimeLevelMember> members = dimModel.getTimeMembers(
					query.getMeasureDimension(), s, e);
			tsc = new TimeSeriesCollection(members.size(), String.class,
					factory);

			int i = 0;
			for (final TimeLevelMember member : members) {
				assert 1 == member.getRanges().size();
				final TimeMemberRange range = member.getRange(0);

				// set the label
				tsc.setLabel(i, member.getName(), member.getId());

				for (final DescriptorMathTree measure : query.getMeasures()) {
					final IMathTreeElement node = ((MathOperatorNode) measure
							.getRoot()).getChild(0);

					if (measure.isSimple()) {
						final List<Map<String, Object>> tpRecords = filter
								.apply(range.getStart(), range.getEnd());
						final List<Double> res = calculateMeasure(tpRecords,
								node);
						assert 1 == res.size();

						// set the value within the series
						final TimeSeries ts = getTimeSeries(tsc,
								measure.getId());
						ts.setValue(i, res.get(0).doubleValue());
					} else {
						final IMathTreeElement iNode = ((MathOperatorNode) node)
								.getChild(0);
						final List<Double> values = new ArrayList<Double>();

						final List<Map<String, Object>> tpRecords;
						if (filter.incSupport()) {
							tpRecords = filter.apply(range.getStart(),
									range.getEnd());
						} else {
							tpRecords = null;
						}

						// get the inner values
						for (long r = range.getStart(); r <= range.getEnd(); r++) {
							final List<Map<String, Object>> rangeRecords;
							rangeRecords = filter.apply(r, r, tpRecords);
							final List<Double> res = calculateMeasure(
									rangeRecords, iNode);
							assert 1 == res.size();

							values.add(res.get(0));
						}

						final TimeSeries ts = getTimeSeries(tsc,
								measure.getId());
						if (values.size() < 1) {
							ts.setValue(i, Double.NaN);
						} else {
							final MathOperator mo = ((MathOperatorNode) node)
									.get();
							assert mo.isFunction();

							final IAggregationFunction func = mo.getFunction();
							ts.setValue(i, applyFunction(func, values));
						}
					}
				}

				i++;
			}
		}

		return tsc;
	}

	private TimeSeries getTimeSeries(final TimeSeriesCollection tsc,
			final String id) {
		TimeSeries ts = tsc.getSeries(id);
		if (ts == null) {
			ts = tsc.createSeries(id);
		}

		return ts;
	}

	private List<Double> calculateMeasure(
			final List<Map<String, Object>> records, final IMathTreeElement node) {

		final List<Double> result = new ArrayList<Double>();
		if (node instanceof net.meisen.dissertation.impl.parser.query.select.measures.DescriptorLeaf) {
			final net.meisen.dissertation.impl.parser.query.select.measures.DescriptorLeaf dLeaf = (net.meisen.dissertation.impl.parser.query.select.measures.DescriptorLeaf) node;

			for (final Map<String, Object> record : records) {

				final Object value = record.get(dLeaf.get());

				if (value instanceof Number) {
					result.add(((Number) value).doubleValue());
				} else if (value == null) {
					result.add(Double.NaN);
				} else {
					result.add(1.0);
				}
			}
		} else if (node instanceof MathOperatorNode) {
			final MathOperatorNode math = (MathOperatorNode) node;

			final List<List<Double>> childrenRes = new ArrayList<List<Double>>();
			for (final IMathTreeElement child : math.getChildren()) {
				childrenRes.add(calculateMeasure(records, child));
			}

			// handle known values
			final MathOperator mo = ((MathOperatorNode) node).get();
			if (mo.isFunction()) {
				if (childrenRes.size() != 1) {
					fail("Invalid definition '" + node + "'");
				}
				final List<Double> childRes = childrenRes.get(0);
				final IAggregationFunction func = mo.getFunction();
				result.add(applyFunction(func, childRes));
			} else {
				final ArithmeticOperator op = mo.getOperator();
				final List<Double> firstRes = childrenRes.get(0);
				final int valueSize = firstRes.size();

				for (int i = 0; i < valueSize; i++) {
					double curResult = firstRes.get(i);
					for (int k = 1; k < childrenRes.size(); k++) {
						curResult = op.apply(curResult,
								childrenRes.get(k).get(i));
					}

					result.add(curResult);
				}
			}
		} else {
			fail("Invalid node '" + node + "' (" + node.getClass().getName()
					+ ")");
		}

		return result;
	}

	private double applyFunction(final IAggregationFunction func,
			final List<Double> values) {
		double tmpRes = Double.NaN;
		if (func instanceof Sum) {
			for (final double res : values) {
				if (Double.isNaN(res)) {
					tmpRes = Double.NaN;
					break;
				} else {
					tmpRes = Double.isNaN(tmpRes) ? res : tmpRes + res;
				}
			}
		} else if (func instanceof Count) {
			tmpRes = values.size();
		} else if (func instanceof Min) {
			for (final double res : values) {
				if (Double.isNaN(res)) {
					tmpRes = Double.NaN;
					break;
				} else {
					tmpRes = Double.isNaN(tmpRes) ? res : (res < tmpRes ? res
							: tmpRes);
				}
			}
		} else if (func instanceof Max) {
			for (final double res : values) {
				if (Double.isNaN(res)) {
					tmpRes = Double.NaN;
					break;
				} else {
					tmpRes = Double.isNaN(tmpRes) ? res : (res > tmpRes ? res
							: tmpRes);
				}
			}
		} else {
			fail("Unsupported aggregation '" + func.getClass().getSimpleName()
					+ "'");
		}

		return tmpRes;
	}

	/**
	 * CleanUp afterwards.
	 */
	@After
	public void cleanUp() {
		loader.unloadAll();
	}

}
