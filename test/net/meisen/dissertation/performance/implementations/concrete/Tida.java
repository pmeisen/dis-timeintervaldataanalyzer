package net.meisen.dissertation.performance.implementations.concrete;

import java.util.List;
import java.util.Map;

import net.meisen.dissertation.impl.datasets.SingleStaticDataSet;
import net.meisen.dissertation.impl.parser.query.select.IntervalRelation;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.parser.query.select.SelectResultRecords;
import net.meisen.dissertation.impl.parser.query.select.SelectResultTimeSeries;
import net.meisen.dissertation.impl.parser.query.select.SelectResultType;
import net.meisen.dissertation.impl.parser.query.select.measures.DescriptorMathTree;
import net.meisen.dissertation.impl.time.series.TimeSeriesCollection;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.parser.query.IQueryFactory;
import net.meisen.dissertation.model.parser.query.IQueryResult;
import net.meisen.dissertation.performance.implementations.BaseImplementation;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@SuppressWarnings("javadoc")
public class Tida extends BaseImplementation<TidaModel> {
	private final static Logger LOG = LoggerFactory.getLogger(Tida.class);

	protected final IQueryFactory queryFactory;

	public Tida(final TidaModel data, final List<Map<String, Object>> records,
			final int initRuns, final int runs, final IQueryFactory queryFactory) {
		this(data, initRuns, runs, queryFactory);
	}

	public Tida(final TidaModel model, final int initRuns, final int runs,
			final IQueryFactory queryFactory) {
		super(model, initRuns, runs, queryFactory, model.getDimensionModel(),
				model.getIndexFactory(), model.getIntervalModel()
						.getTimelineMapper());

		this.queryFactory = queryFactory;
	}

	public void loadRecords(final List<Map<String, Object>> records) {
		loadRecords(data, records);
	}

	@Override
	protected TimeSeriesCollection measure(final SelectQuery query) {
		final SelectResultTimeSeries resTsc = queryFactory.evaluateQuery(query,
				null);
		return resTsc.getTimeSeriesResult();
	}

	public int getCount(final SelectQuery orgQuery) {
		return getCount(queryFactory, orgQuery);
	}

	public void printRecords(final SelectQuery orgQuery) {
		printRecords(queryFactory, orgQuery);
	}

	/**
	 * Get the count of the selected records.
	 * 
	 * @param orgQuery
	 *            the query fired
	 * 
	 * @return the amount of records
	 */
	public static int getCount(final IQueryFactory queryFactory,
			final SelectQuery orgQuery) {
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
			return ((SelectResultRecords) res).getSelectedRecords()
					.determineCardinality();
		} else {
			throw new IllegalStateException("unable to count records");
		}
	}

	public static void printRecords(final IQueryFactory queryFactory,
			final SelectQuery orgQuery) {

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

		query.setCount(false);
		final SelectResultRecords recRes = queryFactory.evaluateQuery(query,
				null);
		final java.util.Iterator<Object[]> it = recRes.iterator();
		while (it.hasNext()) {
			LOG.debug(java.util.Arrays.asList(it.next()).toString());
		}
	}

	public static void loadRecords(final TidaModel data,
			final List<Map<String, Object>> records) {
		data.setBulkLoad(true);
		try {
			for (final Map<String, Object> record : records) {

				final SingleStaticDataSet dataSet = new SingleStaticDataSet(
						record);
				data.loadRecord(dataSet);
			}
		} finally {
			data.setBulkLoad(false);
		}
	}
}
