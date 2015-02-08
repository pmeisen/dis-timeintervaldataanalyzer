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
import net.meisen.dissertation.performance.implementations.helper.IntervalTree;
import net.meisen.dissertation.performance.implementations.helper.IntervalTree.IntervalData;

@SuppressWarnings("javadoc")
public class FilterCreatedIntervalTree extends IntervalTreeBasedImplementation {

	public FilterCreatedIntervalTree(final TidaModel model,
			final List<Map<String, Object>> records, final int initRuns,
			final int runs, final IQueryFactory queryFactory) {
		this(records, initRuns, runs, queryFactory, model.getDimensionModel(),
				model.getIndexFactory(), model.getIntervalModel()
						.getTimelineMapper());
	}

	public FilterCreatedIntervalTree(final List<Map<String, Object>> records,
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

		// create the new IntervalTree for the filtered values
		final List<IntervalData<Integer>> filteredList = new ArrayList<IntervalData<Integer>>();
		int counter = 0;
		for (final Map<String, Object> filteredRecord : filteredRecords) {

			// create the stuff for the interval-tree
			final long start = mapper.mapToLong(filteredRecord
					.get("INTERVAL_START"));
			final long end = mapper.mapToLong(filteredRecord
					.get("INTERVAL_END"));
			filteredList.add(new IntervalData<Integer>(start, end, counter));

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

		return calculateMeasures(query, filter);
	}

}
