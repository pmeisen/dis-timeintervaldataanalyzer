package net.meisen.dissertation.performance.implementations;

import net.meisen.dissertation.help.Performance;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.time.series.TimeSeries;
import net.meisen.dissertation.impl.time.series.TimeSeriesCollection;
import net.meisen.dissertation.model.data.DimensionModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.parser.query.IQueryFactory;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.dissertation.performance.PerformanceResult;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@SuppressWarnings("javadoc")
public abstract class BaseImplementation<T> implements IImplementation {
	private final static Logger LOG = LoggerFactory
			.getLogger(BaseImplementation.class);

	protected final int runs;
	protected final int initRuns;

	protected final DimensionModel dimModel;
	protected final BaseIndexFactory factory;
	protected final BaseMapper<?> mapper;

	protected final IQueryFactory queryFactory;

	protected final T data;

	public BaseImplementation(final TidaModel model, final T data,
			final int initRuns, final int runs, final IQueryFactory queryFactory) {
		this(data, initRuns, runs, queryFactory, model.getDimensionModel(),
				model.getIndexFactory(), model.getIntervalModel()
						.getTimelineMapper());
	}

	public BaseImplementation(final T data, final int initRuns, final int runs,
			final IQueryFactory queryFactory, final DimensionModel dimModel,
			final BaseIndexFactory factory, final BaseMapper<?> mapper) {
		this.runs = runs;
		this.initRuns = initRuns;

		this.data = data;

		this.queryFactory = queryFactory;
		this.dimModel = dimModel;
		this.factory = factory;
		this.mapper = mapper;
	}

	protected TimeSeries getTimeSeries(final TimeSeriesCollection tsc,
			final String id) {
		TimeSeries ts = tsc.getSeries(id);
		if (ts == null) {
			ts = tsc.createSeries(id);
		}

		return ts;
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
		if (LOG.isWarnEnabled()) {
			long sum = 0;
			long min = Long.MAX_VALUE;
			long max = Long.MIN_VALUE;
			for (int l = 0; l < runs; l++) {
				sum += results[l];
				min = results[l] < min ? results[l] : min;
				max = results[l] > max ? results[l] : max;
			}

			LOG.warn("- avg result: " + (sum / runs));
			LOG.warn("- min result: " + min);
			LOG.warn("- max result: " + max);
		}
	}

	protected void setResults(final long[] results, final PerformanceResult pRes) {
		long sum = 0;
		long min = Long.MAX_VALUE;
		long max = Long.MIN_VALUE;
		for (int l = 0; l < runs; l++) {
			sum += results[l];
			min = results[l] < min ? results[l] : min;
			max = results[l] > max ? results[l] : max;
		}

		pRes.runs = runs;
		pRes.avg = (sum / (runs == 0 ? 1 : runs));
		pRes.min = min;
		pRes.max = max;
	}

	@Override
	public TimeSeriesCollection run(final String query) {
		return measure(queryFactory.<SelectQuery> parseQuery(query));
	}

	@Override
	public TimeSeriesCollection run(final SelectQuery query,
			final PerformanceResult pRes) {
		final long[] results = new long[runs];

		// do some parsing
		Performance performance;

		// iteration
		TimeSeriesCollection tsc = null;
		for (int i = 0; i < runs + initRuns; i++) {
			performance = new Performance();
			if (i >= initRuns) {
				performance.start(false);
			}

			tsc = measure(query);

			if (i >= initRuns) {
				results[i - initRuns] = performance.stop(false)[0];
			}
		}

		// result
		setResults(results, pRes);
		System.gc();

		return tsc;
	}

	protected abstract TimeSeriesCollection measure(final SelectQuery query);
}
