package net.meisen.dissertation.performance.implementations;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import net.meisen.dissertation.model.data.DimensionModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.parser.query.IQueryFactory;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.dissertation.performance.implementations.helper.IntervalTree;
import net.meisen.dissertation.performance.implementations.helper.IntervalTree.IntervalData;

@SuppressWarnings("javadoc")
public abstract class IntervalTreeBasedImplementation extends
		RecordBasedImplementation {
	protected final IntervalTree<Integer> iTree;

	public IntervalTreeBasedImplementation(final TidaModel model,
			final List<Map<String, Object>> records, final int initRuns,
			final int runs, final IQueryFactory queryFactory) {
		this(records, initRuns, runs, queryFactory, model.getDimensionModel(),
				model.getIndexFactory(), model.getIntervalModel()
						.getTimelineMapper());
	}

	public IntervalTreeBasedImplementation(
			final List<Map<String, Object>> records, final int initRuns,
			final int runs, final IQueryFactory queryFactory,
			final DimensionModel dimModel, final BaseIndexFactory factory,
			final BaseMapper<?> mapper) {
		super(records, "INTERVAL_START", "INTERVAL_END", initRuns, runs,
				queryFactory, dimModel, factory, mapper);

		int i = 0;
		final List<IntervalData<Integer>> list = new ArrayList<IntervalData<Integer>>();

		for (final Map<String, Object> record : records) {
			final Object rStart = record.get("INTERVAL_START");
			final Object rEnd = record.get("INTERVAL_END");

			final long start = mapper.mapToLong(rStart);
			final long end = mapper.mapToLong(rEnd);

			list.add(new IntervalData<Integer>(start, end, i));
			i++;
		}

		iTree = new IntervalTree<Integer>(list);
	}
}
