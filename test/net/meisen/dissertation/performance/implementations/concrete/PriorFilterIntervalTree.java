package net.meisen.dissertation.performance.implementations.concrete;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.time.series.TimeSeriesCollection;
import net.meisen.dissertation.model.data.DimensionModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.parser.query.IQueryFactory;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.dissertation.performance.implementations.IRecordsFilter;
import net.meisen.dissertation.performance.implementations.IntervalTreeBasedImplementation;
import net.meisen.dissertation.performance.implementations.helper.IntervalTree.IntervalData;

@SuppressWarnings("javadoc")
public class PriorFilterIntervalTree extends IntervalTreeBasedImplementation {

	public PriorFilterIntervalTree(final TidaModel model,
			final List<Map<String, Object>> records, final int initRuns,
			final int runs, final IQueryFactory queryFactory) {
		this(records, initRuns, runs, queryFactory, model.getDimensionModel(),
				model.getIndexFactory(), model.getIntervalModel()
						.getTimelineMapper());
	}

	public PriorFilterIntervalTree(final List<Map<String, Object>> records,
			final int initRuns, final int runs,
			final IQueryFactory queryFactory, final DimensionModel dimModel,
			final BaseIndexFactory factory, final BaseMapper<?> mapper) {
		super(records, initRuns, runs, queryFactory, dimModel, factory, mapper);
	}

	@Override
	protected TimeSeriesCollection measure(final SelectQuery query) {
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

		return calculateMeasures(query, filter);
	}

}
