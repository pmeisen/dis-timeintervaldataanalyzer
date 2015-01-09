package net.meisen.dissertation.performance.paper.in2014.iceis;

import java.io.IOException;
import java.sql.SQLException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.TimeUnit;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.Db;
import net.meisen.dissertation.help.ModuleAndDbBasedTest;
import net.meisen.dissertation.help.Performance;
import net.meisen.dissertation.impl.datasets.SingleStaticDataSet;
import net.meisen.dissertation.impl.parser.query.select.SelectResultRecords;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IQueryFactory;
import net.meisen.dissertation.model.parser.query.IQueryResult;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.dissertation.performance.paper.in2014.iceis.naive.IntervalTree;
import net.meisen.dissertation.performance.paper.in2014.iceis.naive.IntervalTree.IntervalData;
import net.meisen.general.genmisc.raster.configuration.IRasterConfiguration;
import net.meisen.general.genmisc.raster.configuration.impl.BaseRasterConfiguration;
import net.meisen.general.genmisc.raster.data.impl.BaseModelData;
import net.meisen.general.genmisc.raster.definition.IRaster;
import net.meisen.general.genmisc.raster.definition.IRasterLogic;
import net.meisen.general.genmisc.raster.definition.IRasterModel;
import net.meisen.general.genmisc.raster.definition.RasterModelEntryType;
import net.meisen.general.genmisc.raster.definition.impl.BaseRaster;
import net.meisen.general.genmisc.raster.definition.impl.BaseRasterModel;
import net.meisen.general.genmisc.raster.definition.impl.BaseRasterModelEntry;
import net.meisen.general.genmisc.raster.definition.impl.date.DateGranularity;
import net.meisen.general.genmisc.raster.definition.impl.date.DateRasterGranularity;
import net.meisen.general.genmisc.raster.definition.impl.date.DateRasterLogic;
import net.meisen.general.genmisc.raster.function.IRasterFunction;
import net.meisen.general.genmisc.raster.function.impl.Count;
import net.meisen.general.genmisc.raster.function.impl.IntervalSum;
import net.meisen.general.genmisc.raster.function.impl.Value;
import net.meisen.general.genmisc.types.Dates;
import net.meisen.general.genmisc.types.Objects;
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
	private final static int RUNS = 100;

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
		final TidaModel model = loader
				.loadViaXslt("/net/meisen/dissertation/performance/paper/in2014/iceis/model/tida-model-minute.xml");
		final BaseMapper<?> mapper = model.getIntervalModel()
				.getTimelineMapper();

		// get the data
		final List<Map<String, Object>> rows = db
				.query("tida",
						"SELECT KEY, PERSON, TASKTYPE, WORKAREA, INTERVAL_START, INTERVAL_END FROM SMC_DATA LIMIT "
								+ limit);

		// add the data to the model, the interval-tree and the map is naive one
		final List<IntervalData<Integer>> list = new ArrayList<IntervalData<Integer>>();
		model.setBulkLoad(true);
		try {
			int i = 0;
			for (final Map<String, Object> row : rows) {

				// create the stuff for the interval-tree
				final long start = mapper.mapToLong(row.get("INTERVAL_START"));
				final long end = mapper.mapToLong(row.get("INTERVAL_END"));
				list.add(new IntervalData<Integer>(start, end, i));

				final SingleStaticDataSet dataSet = new SingleStaticDataSet(row);
				model.loadRecord(dataSet);

				i++;
			}
		} finally {
			model.setBulkLoad(false);
		}
		final IntervalTree<Integer> iTree = new IntervalTree<Integer>(list);

		String start, end, aggr;
		Map<String, Object> filter = new HashMap<String, Object>();
		filter.put("WORKAREA", "SEN.W10");
		filter.put("WORKAREA", "SEN.W11");
		filter.put("WORKAREA", "SEN.W12");
		filter.put("WORKAREA", "SEN.W13");
		filter.put("WORKAREA", "SEN.W14");
		filter.put("WORKAREA", "SEN.W15");
		filter.put("WORKAREA", "SEN.W16");
		filter.put("WORKAREA", "SEN.W17");
		filter.put("WORKAREA", "SEN.W18");
		filter.put("WORKAREA", "SEN.W19");
		filter.put("WORKAREA", "SEN.W20");
		filter.put("WORKAREA", "SEN.W21");
		filter.put("WORKAREA", "SEN.W22");
		filter.put("WORKAREA", "SEN.W27");
		filter.put("WORKAREA", "SEN.W28");
		filter.put("WORKAREA", "SEN.W31");

		/*
		 * 1. Test: OF COUNT() IN [01.JAN, 02.JAN) WHERE WA.LOC.TYPE="Gate"
		 */
		start = "01.01.2008 00:00:00";
		end = "01.01.2008 23:59:00";
		aggr = "COUNT";
		filter.clear();

		measureNaive(rows, mapper, start, end, aggr, filter);
		measureIntervalTree(iTree, rows, mapper, start, end, aggr, filter);
		measureTidaModel(model, mapper, start, end, aggr, filter);
		getCount(model, mapper, start, end, aggr, filter);

		/*
		 * 2. Test: OF MAX ON TIME.5ToY.DAY OF SUM(PERSON) IN [01.JAN, 08.JAN)
		 * WHERE WORKA-REA="SEN.W14"
		 */
		start = "01.01.2008 00:00:00";
		end = "07.01.2008 23:59:00";
		aggr = "SUM";
		filter.clear();
		filter.put("WORKAREA", "SEN.W14");

		measureNaive(rows, mapper, start, end, aggr, filter);
		measureIntervalTree(iTree, rows, mapper, start, end, aggr, filter);
		measureTidaModel(model, mapper, start, end, aggr, filter);
		getCount(model, mapper, start, end, aggr, filter);

		/*
		 * 3. Test: OF MAX ON TIME.5ToY.YEAR OF SUM(PERSON) IN [01.JAN, 15.JAN)
		 * WHERE TASKTYPE="short" GROUP BY PER.QUAL.DEG
		 */
		start = "01.01.2008 00:00:00";
		end = "14.01.2008 23:59:00";
		aggr = "SUM";
		filter.clear();
		filter.put("TASKTYPE", "short");

		measureNaive(rows, mapper, start, end, aggr, filter);
		measureIntervalTree(iTree, rows, mapper, start, end, aggr, filter);
		measureTidaModel(model, mapper, start, end, aggr, filter);
		getCount(model, mapper, start, end, aggr, filter);

		// cleanUp
		loader.unloadAll();
		db.shutDownDb();
	}

	/**
	 * Test the performance of a naive implemenation.
	 * 
	 * @param data
	 *            the data
	 * @param mapper
	 *            the mapper to be used
	 * @param startDate
	 *            the start date
	 * @param endDate
	 *            the end date
	 * @param aggregation
	 *            the aggregation to be used
	 * @param filter
	 *            the filter defined
	 * @throws ParseException
	 *             if an error occurres
	 */
	protected void measureNaive(final List<Map<String, Object>> data,
			final BaseMapper<?> mapper, final String startDate,
			final String endDate, final String aggregation,
			final Map<String, Object> filter) throws ParseException {
		final long[] results = new long[RUNS];

		final Map<Date, IRaster<Date>> rasters = createRasters(startDate,
				endDate);

		Performance performance;

		// iteration
		for (int i = 0; i < RUNS; i++) {
			performance = new Performance();
			performance.start();

			for (final Map<String, Object> row : data) {

				// check the filter
				boolean valid = filter.size() == 0;
				for (final Entry<String, Object> entry : filter.entrySet()) {
					if (Objects.equals(row.get(entry.getKey()),
							entry.getValue())) {
						valid = true;
						break;
					}
				}
				if (!valid) {
					continue;
				}

				final BaseModelData d = new BaseModelData();
				d.setValues(row);

				// find the raster
				final Date day = Dates.truncateDate((Date) row
						.get("INTERVAL_START"));
				final IRaster<Date> raster = rasters.get(day);
				if (raster == null) {
					// System.out.println("SKIP: " + row);
				} else {
					raster.addModelData(d);
				}
			}

			results[i] = performance.stop()[0];
		}

		// result
		printResult("Naive", results);
	}

	/**
	 * Measure the performance of the interval-tree.
	 * 
	 * @param iTree
	 *            the interval tree
	 * @param data
	 *            the data
	 * @param mapper
	 *            the mapper to be used
	 * @param startDate
	 *            the start date
	 * @param endDate
	 *            the end date
	 * @param aggregation
	 *            the aggregation to be used
	 * @param filter
	 *            the filter defined
	 * 
	 * @throws ParseException
	 *             if an error occures
	 */
	protected void measureIntervalTree(final IntervalTree<Integer> iTree,
			final List<Map<String, Object>> data, final BaseMapper<?> mapper,
			final String startDate, final String endDate,
			final String aggregation, final Map<String, Object> filter)
			throws ParseException {
		final long[] results = new long[RUNS];
		final Map<Date, IRaster<Date>> rasters = createRasters(startDate,
				endDate);

		Performance performance;

		// iteration
		for (int i = 0; i < RUNS; i++) {
			performance = new Performance();
			performance.start();

			final IntervalData<Integer> res = iTree.query(mapper
					.mapToLong(Dates.createDateFromString(startDate,
							"dd.MM.yyyy HH:mm:ss")), mapper.mapToLong(Dates
					.createDateFromString(endDate, "dd.MM.yyyy HH:mm:ss")));
			final Collection<Integer> positions = res == null ? Collections
					.<Integer> emptyList() : res.getData();

			for (final Integer pos : positions) {
				final Map<String, Object> row = data.get(pos);

				// check the filter
				boolean valid = filter.size() == 0;
				for (final Entry<String, Object> entry : filter.entrySet()) {
					if (Objects.equals(row.get(entry.getKey()),
							entry.getValue())) {
						valid = true;
						break;
					}
				}
				if (!valid) {
					continue;
				}

				final BaseModelData d = new BaseModelData();
				d.setValues(row);

				// find the raster
				final Date day = Dates.truncateDate((Date) row
						.get("INTERVAL_START"));
				final IRaster<Date> raster = rasters.get(day);
				if (raster == null) {
					// System.out.println("SKIP: " + row);
				} else {
					raster.addModelData(d);
				}
			}

			results[i] = performance.stop()[0];
		}

		// result
		printResult("IntervalTree", results);
	}

	/**
	 * Function to test the performance of the TidaModel.
	 * 
	 * @param model
	 *            the model to be tested
	 * @param mapper
	 *            the mapper to be used
	 * @param startDate
	 *            the start date
	 * @param endDate
	 *            the end date
	 * @param aggregation
	 *            the aggregation to be used
	 * @param filter
	 *            the filter defined
	 */
	protected void measureTidaModel(final TidaModel model,
			final BaseMapper<?> mapper, final String startDate,
			final String endDate, final String aggregation,
			final Map<String, Object> filter) {
		final long[] results = new long[RUNS];

		Performance performance;

		// parsing
		String filterQuery = "";
		for (Entry<String, Object> e : filter.entrySet()) {
			filterQuery += (filterQuery.isEmpty() ? " WHERE " : " OR ")
					+ e.getKey() + "='" + e.getValue() + "'";
		}
		final IQuery query = queryFactory.parseQuery("select timeseries of "
				+ aggregation + "(PERSON) from tidaModel in [" + startDate
				+ "," + endDate + "]" + filterQuery);

		// iteration
		for (int i = 0; i < RUNS; i++) {
			performance = new Performance();
			performance.start();

			queryFactory.evaluateQuery(query, null);

			results[i] = performance.stop()[0];
		}

		// result
		printResult("TidaModel", results);
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
	 * @param mapper
	 *            the mapper
	 * @param startDate
	 *            the start date
	 * @param endDate
	 *            the end date
	 * @param aggregation
	 *            the aggregation function to be used
	 * @param filter
	 *            the filter defined
	 */
	protected void getCount(final TidaModel model, final BaseMapper<?> mapper,
			final String startDate, final String endDate,
			final String aggregation, final Map<String, Object> filter) {
		String filterQuery = "";
		for (Entry<String, Object> e : filter.entrySet()) {
			filterQuery += (filterQuery.isEmpty() ? " WHERE " : " OR ")
					+ e.getKey() + "='" + e.getValue() + "'";
		}
		final String queryString = "select COUNT(RECORDS) from tidaModel WITHIN ["
				+ startDate + "," + endDate + "]" + filterQuery;

		final IQuery query = queryFactory.parseQuery(queryString);
		final IQueryResult res = queryFactory.evaluateQuery(query, null);
		if (res instanceof SelectResultRecords) {
			System.out.println("COUNT: "
					+ ((SelectResultRecords) res).getSelectedRecords()
							.determineCardinality());
		} else {
			System.out.println("COUNT: INVALID RESULT!");
		}
	}

	/**
	 * CleanUp afterwards.
	 */
	@After
	public void cleanUp() {
		loader.unloadAll();
	}

	private Map<Date, IRaster<Date>> createRasters(final String startDate,
			final String endDate) throws ParseException {
		final Date startDay = Dates.truncateDate(Dates.createDateFromString(
				startDate, "dd.MM.yyyy HH:mm:ss"));
		final Date endDay = Dates.truncateDate(Dates.createDateFromString(
				endDate, "dd.MM.yyyy HH:mm:ss"));

		// create a raster
		Map<Date, IRaster<Date>> rasters = new HashMap<Date, IRaster<Date>>();
		final long diff = TimeUnit.DAYS.convert(
				endDay.getTime() - startDay.getTime(), TimeUnit.MILLISECONDS);
		for (long i = 0; i <= diff; i++) {
			final IRaster<Date> raster = createRasterWithModel(
					DateGranularity.MINUTES, 1, Locale.US);
			final Date day = new Date(startDay.getTime() + i * 86400000);
			rasters.put(day, raster);
		}

		return rasters;
	}

	private IRaster<Date> createRasterWithModel(
			final DateGranularity granularity, final int bucketSize,
			final Locale locale) {

		// create the models
		final Map<String, IRasterModel> models = new HashMap<String, IRasterModel>();
		final Object[] paramStart = { "INTERVAL_START" };
		final Object[] paramEnd = { "INTERVAL_END" };

		final BaseRasterModel model = (BaseRasterModel) createRasterModel(
				"INTERVAL_START", "INTERVAL_END", new Value(), new Value(),
				paramStart, paramEnd);
		model.addEntry(new BaseRasterModelEntry("COUNT",
				RasterModelEntryType.VALUE, new Count()));
		model.addEntry(new BaseRasterModelEntry("SUM",
				RasterModelEntryType.VALUE, new IntervalSum()));
		models.put("TESTMODEL", model);

		// create the raster
		final IRaster<Date> raster = createDateRaster(granularity, bucketSize,
				locale, models);

		return raster;
	}

	private IRasterModel createRasterModel(final String nameStart,
			final String nameEnd, final IRasterFunction functionStart,
			final IRasterFunction functionEnd, final Object[] parametersStart,
			final Object[] parametersEnd) {

		// create the interval boundaries
		final BaseRasterModelEntry entryStart = new BaseRasterModelEntry(
				nameStart, RasterModelEntryType.INTERVALSTART, functionStart,
				parametersStart);
		final BaseRasterModelEntry entryEnd = new BaseRasterModelEntry(nameEnd,
				RasterModelEntryType.INTERVALEND, functionEnd, parametersEnd);

		// create the model
		final BaseRasterModel model = new BaseRasterModel(entryStart, entryEnd);

		return model;
	}

	private IRaster<Date> createDateRaster(final DateGranularity granularity,
			final Integer bucketSize, final Locale locale,
			final Map<String, IRasterModel> models) {
		final IRasterConfiguration<Date> configuration = createDateRasterConfiguration(
				granularity, bucketSize, locale);

		// add some models to the configuration
		final BaseRasterConfiguration<Date> dateConfiguration = (BaseRasterConfiguration<Date>) configuration;

		// add the models
		if (models != null) {
			for (final Entry<String, IRasterModel> entry : models.entrySet()) {
				dateConfiguration.addModel(entry.getKey(), entry.getValue());
			}
		}

		// create the raster
		final IRaster<Date> raster = new BaseRaster<Date>(configuration);

		return raster;
	}

	private IRasterConfiguration<Date> createDateRasterConfiguration(
			final DateGranularity granularity, final Integer bucketSize,
			final Locale locale) {

		final IRasterLogic<Date> dateRasterLogic = createDateRasterLogic(
				granularity, bucketSize);
		final BaseRasterConfiguration<Date> configuration = new BaseRasterConfiguration<Date>(
				dateRasterLogic);
		configuration.setLocale(locale);

		return configuration;
	}

	private IRasterLogic<Date> createDateRasterLogic(
			final DateGranularity granularity, final Integer bucketSize) {

		final DateRasterGranularity dateRasterGranularity = new DateRasterGranularity(
				granularity, bucketSize);
		final DateRasterLogic dateRasterLogic = new DateRasterLogic(
				dateRasterGranularity);

		return dateRasterLogic;
	}
}
