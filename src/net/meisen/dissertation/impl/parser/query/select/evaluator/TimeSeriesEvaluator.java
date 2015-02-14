package net.meisen.dissertation.impl.parser.query.select.evaluator;

import java.util.Collection;
import java.util.Iterator;
import java.util.Set;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.impl.measures.Count;
import net.meisen.dissertation.impl.parser.query.DimensionSelector;
import net.meisen.dissertation.impl.parser.query.Interval;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.parser.query.select.SelectResult;
import net.meisen.dissertation.impl.parser.query.select.SelectResultTimeSeries;
import net.meisen.dissertation.impl.parser.query.select.measures.DescriptorMathTree;
import net.meisen.dissertation.impl.time.series.TimeSeries;
import net.meisen.dissertation.impl.time.series.TimeSeriesCollection;
import net.meisen.dissertation.model.data.DimensionModel;
import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.dissertation.model.dimensions.TimeMemberRange;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorModelSet;
import net.meisen.dissertation.model.indexes.datarecord.slices.SliceWithDescriptors;
import net.meisen.dissertation.model.measures.IDimAggregationFunction;
import net.meisen.dissertation.model.measures.IMathAggregationFunction;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Numbers;

/**
 * Evaluator used to evaluate timeseries.
 * 
 * @author pmeisen
 * 
 */
public class TimeSeriesEvaluator {
	private static final DescriptorMathTree defaultMeasure = new DescriptorMathTree(
			"COUNT");
	static {
		defaultMeasure.attach(new Count());
		defaultMeasure.attach("");
	}

	private static final class CombinedSlices {
		private final Bitmap combinedBitmap;
		private final FactDescriptorModelSet combinedFacts;

		public CombinedSlices(final Bitmap combinedBitmap,
				final FactDescriptorModelSet combinedFacts) {
			this.combinedBitmap = combinedBitmap;
			this.combinedFacts = combinedFacts;
		}

		public Bitmap getBitmap() {
			return combinedBitmap;
		}

		public FactDescriptorModelSet getFacts() {
			return combinedFacts;
		}
	}

	private static final Iterable<GroupResultEntry> noGroupIterable = new Iterable<GroupResultEntry>() {

		@Override
		public Iterator<GroupResultEntry> iterator() {
			return new Iterator<GroupResultEntry>() {
				private boolean read = false;

				@Override
				public boolean hasNext() {
					return !read;
				}

				@Override
				public GroupResultEntry next() {
					read = true;
					return null;
				}

				@Override
				public void remove() {
					throw new IllegalStateException("Not supported, read only!");
				}
			};
		}
	};

	private static final Iterable<DescriptorMathTree> noMeasureIterable = new Iterable<DescriptorMathTree>() {

		@Override
		public Iterator<DescriptorMathTree> iterator() {
			return new Iterator<DescriptorMathTree>() {
				private boolean read = false;

				@Override
				public boolean hasNext() {
					return !read;
				}

				@Override
				public DescriptorMathTree next() {
					read = true;
					return defaultMeasure;
				}

				@Override
				public void remove() {
					throw new IllegalStateException("Not supported, read only!");
				}
			};
		}
	};

	/**
	 * The {@code TidaIndex} of the model specified during construction.
	 */
	protected final TidaIndex index;
	/**
	 * The {@code IndexFactory} of the model specified during construction.
	 */
	protected final BaseIndexFactory indexFactory;
	/**
	 * The {@code IntervalModel} of the model specified during construction.
	 */
	protected final IntervalModel intervalModel;
	/**
	 * The {@code DimensionModel} of the model specified during construction.
	 */
	protected final DimensionModel dimModel;

	/**
	 * Standard constructor initializing the {@code TimeSeriesEvaluator} for the
	 * specified {@code model}.
	 * 
	 * @param model
	 *            the model the evaluator is used with
	 */
	public TimeSeriesEvaluator(final TidaModel model) {

		this.dimModel = model.getDimensionModel();
		this.index = model.getIndex();
		this.indexFactory = model.getIndexFactory();
		this.intervalModel = model.getIntervalModel();
	}

	/**
	 * Evaluates the result for the specified {@code interval}.
	 * 
	 * @param query
	 *            the query to create the result for
	 * @param queryResult
	 *            the result of the parsing and interpretation of the select
	 *            statement
	 * 
	 * @return the created {@code TimeSeries}
	 * 
	 * @see TimeSeries
	 * @see SelectResultTimeSeries
	 */
	public TimeSeriesCollection evaluateInterval(final SelectQuery query,
			final SelectResult queryResult) {

		// get the result for each group
		final GroupResult filteredGroupResult = queryResult
				.getFilteredGroupResult();

		// depending on the dimensions we have to choose what to do
		if (query.getMeasureDimension() == null) {
			return evaluateLow(query, filteredGroupResult);
		} else if (!query.usesFunction(IMathAggregationFunction.class)) {
			return evaluateDim(query, filteredGroupResult);
		} else if (!query.usesFunction(IDimAggregationFunction.class)) {
			return evaluateMath(query, filteredGroupResult);
		} else {
			return evaluateMixed(query, filteredGroupResult);
		}
	}

	/**
	 * Helper method to create a non-null iteratable used to iterate over the
	 * measures.
	 * 
	 * @param measures
	 *            the measures to iterate over
	 * 
	 * @return an iterator, which cannot be {@code null}
	 */
	protected final Iterable<DescriptorMathTree> it(
			final Collection<DescriptorMathTree> measures) {
		if (measures == null || measures.size() == 0) {
			return noMeasureIterable;
		} else {
			return measures;
		}
	}

	/**
	 * Helper method to create a non-null iteratable used to iterate over the
	 * result of grouping.
	 * 
	 * @param groupResult
	 *            the {@code GroupResult} to iterate over
	 * 
	 * @return an iterator, which cannot be {@code null}
	 */
	protected final Iterable<GroupResultEntry> it(final GroupResult groupResult) {
		if (groupResult == null || groupResult.size() == 0) {
			return noGroupIterable;
		} else {
			return groupResult;
		}
	}

	/**
	 * Creates a combination for the specified members, i.e. a combined
	 * {@code FactDescriptorModelSet} and a combined bitmap.
	 * 
	 * @param member
	 *            the members to be combined
	 * @param bounds
	 *            the range limiting the members
	 * 
	 * @return the combination
	 */
	protected CombinedSlices createCombination(final TimeLevelMember member,
			final long[] bounds) {

		// iterate over the ranges and create combined bitmaps and facts
		final FactDescriptorModelSet combinedFactSet = new FactDescriptorModelSet();
		Bitmap combinedBitmap = null;
		for (final TimeMemberRange range : member.getRanges()) {
			final long start = Math.max(bounds[0], range.getStart());
			final long end = Math.min(bounds[1], range.getEnd());

			// get the slices for the range
			final SliceWithDescriptors<?>[] slices = index
					.getIntervalIndexSlices(start, end);

			// combine the slices and facts
			for (final SliceWithDescriptors<?> slice : slices) {
				if (slice == null) {
					continue;
				}

				final FactDescriptorModelSet factSet = slice.getFactsSet();
				combinedFactSet.combine(factSet);
				combinedBitmap = Bitmap.combineBitmaps(combinedBitmap, slice);
			}
		}

		return new CombinedSlices(combinedBitmap, combinedFactSet);
	}

	/**
	 * Evaluates the query's measures if and only if there are mixed-functions
	 * used.
	 * 
	 * @param query
	 *            the query to be evaluated
	 * @param filteredGroupResult
	 *            the filtered group result
	 * 
	 * @return the created result
	 */
	protected TimeSeriesCollection evaluateMixed(final SelectQuery query,
			final GroupResult filteredGroupResult) {

		// get some stuff we need
		final Interval<?> interval = query.getInterval();
		final Iterable<DescriptorMathTree> itMeasures = it(query.getMeasures());
		final Iterable<GroupResultEntry> itGroup = it(filteredGroupResult);

		final DimensionSelector dim = query.getMeasureDimension();
		final long[] bounds = getBounds(interval);

		// get the members of the selected level
		final Set<TimeLevelMember> members = dimModel.getTimeMembers(dim,
				bounds[0], bounds[1]);

		// create the timeSeries
		final TimeSeriesCollection result = createTimeSeriesResult(members);

		int timeSeriesPos = 0;
		for (final TimeLevelMember member : members) {

			// iterate over the ranges and create combined bitmaps and facts
			final CombinedSlices combined = createCombination(member, bounds);
			final Bitmap timeBitmap = combined.getBitmap();

			for (final GroupResultEntry groupResultEntry : itGroup) {
				final String groupId = createGroupId(groupResultEntry);
				final Bitmap resultBitmap = Bitmap.combineTimeAndFilter(
						indexFactory, timeBitmap, groupResultEntry);

				// create the evaluator
				final ExpressionEvaluator evaluator = new MixedExpressionEvaluator(
						index, bounds, member.getRanges(),
						groupResultEntry.getBitmap(), resultBitmap,
						combined.getFacts());

				// calculate each measure
				if (!fillTimeSeries(groupId, timeSeriesPos, evaluator, result,
						itMeasures)) {
					return null;
				}
			}

			timeSeriesPos++;
		}

		return result;
	}

	/**
	 * Evaluates the query's measures if and only if there are only
	 * math-functions.
	 * 
	 * @param query
	 *            the query to be evaluated
	 * @param filteredGroupResult
	 *            the filtered group result
	 * 
	 * @return the created result
	 */
	protected TimeSeriesCollection evaluateMath(final SelectQuery query,
			final GroupResult filteredGroupResult) {

		// get some stuff we need
		final Interval<?> interval = query.getInterval();
		final Iterable<DescriptorMathTree> itMeasures = it(query.getMeasures());
		final Iterable<GroupResultEntry> itGroup = it(filteredGroupResult);

		final DimensionSelector dim = query.getMeasureDimension();
		final long[] bounds = getBounds(interval);

		// get the members of the selected level
		final Set<TimeLevelMember> members = dimModel.getTimeMembers(dim,
				bounds[0], bounds[1]);

		// create the timeSeries
		final TimeSeriesCollection result = createTimeSeriesResult(members);

		int timeSeriesPos = 0;
		for (final TimeLevelMember member : members) {
			for (final GroupResultEntry groupResultEntry : itGroup) {
				final String groupId = createGroupId(groupResultEntry);

				// create the evaluator
				final ExpressionEvaluator evaluator = new MathExpressionEvaluator(
						index, bounds, member.getRanges(),
						groupResultEntry.getBitmap());

				// calculate each measure
				if (!fillTimeSeries(groupId, timeSeriesPos, evaluator, result,
						itMeasures)) {
					return null;
				}
			}

			timeSeriesPos++;
		}

		return result;
	}

	/**
	 * Evaluates the query's measures if and only if there are only
	 * dim-functions.
	 * 
	 * @param query
	 *            the query to be evaluated
	 * @param filteredGroupResult
	 *            the filtered group result
	 * 
	 * @return the created result
	 */
	protected TimeSeriesCollection evaluateDim(final SelectQuery query,
			final GroupResult filteredGroupResult) {

		// get some stuff we need
		final Interval<?> interval = query.getInterval();
		final Iterable<DescriptorMathTree> itMeasures = it(query.getMeasures());
		final Iterable<GroupResultEntry> itGroup = it(filteredGroupResult);

		final DimensionSelector dim = query.getMeasureDimension();
		final long[] bounds = getBounds(interval);

		// get the members of the selected level
		final Set<TimeLevelMember> members = dimModel.getTimeMembers(dim,
				bounds[0], bounds[1]);

		// create the timeSeries
		final TimeSeriesCollection result = createTimeSeriesResult(members);

		int timeSeriesPos = 0;
		for (final TimeLevelMember member : members) {

			// iterate over the ranges and create combined bitmaps and facts
			final CombinedSlices combined = createCombination(member, bounds);
			final Bitmap timeBitmap = combined.getBitmap();

			for (final GroupResultEntry groupResultEntry : itGroup) {
				final String groupId = createGroupId(groupResultEntry);

				// combine the bitmaps: filter, member, valid with the group
				final Bitmap resultBitmap = Bitmap.combineTimeAndFilter(
						indexFactory, timeBitmap, groupResultEntry);

				// create the evaluator
				final DimExpressionEvaluator evaluator = new DimExpressionEvaluator(
						index, resultBitmap, combined.getFacts());

				// calculate each measure
				if (!fillTimeSeries(groupId, timeSeriesPos, evaluator, result,
						itMeasures)) {
					return null;
				}
			}

			timeSeriesPos++;
		}

		return result;
	}

	/**
	 * Evaluates the query's measures if and only if there are only
	 * low-functions.
	 * 
	 * @param query
	 *            the query to be evaluated
	 * @param filteredGroupResult
	 *            the filtered result for each group
	 * 
	 * @return the created result
	 */
	protected TimeSeriesCollection evaluateLow(final SelectQuery query,
			final GroupResult filteredGroupResult) {

		// get some stuff we need
		final Interval<?> interval = query.getInterval();
		final Iterable<DescriptorMathTree> itMeasures = it(query.getMeasures());
		final Iterable<GroupResultEntry> itGroup = it(filteredGroupResult);

		// determine the window and get the slices needed
		final long[] bounds = getBounds(interval);
		final SliceWithDescriptors<?>[] timeSlices;
		if (bounds == null) {
			timeSlices = new SliceWithDescriptors<?>[0];
		} else {
			timeSlices = index.getIntervalIndexSlices(bounds[0], bounds[1]);
		}

		// determine the boundaries
		final Object startPoint;
		final boolean startInclusive;
		if (interval == null) {
			startInclusive = true;
			startPoint = intervalModel.getTimelineDefinition().getStart();
		} else {
			startInclusive = interval.getOpenType().isInclusive();
			startPoint = interval.getStart();
		}

		// create the result
		final TimeSeriesCollection result = createTimeSeriesResult(startPoint,
				startInclusive, timeSlices.length);

		/*
		 * Iterate over the different slices and calculate each measure.
		 */
		int offset = bounds == null ? 0 : Numbers.castToInt(bounds[0]);
		int timeSeriesPos = 0;
		for (final SliceWithDescriptors<?> timeSlice : timeSlices) {
			final Bitmap timeBitmap;
			final FactDescriptorModelSet factSet;

			if (timeSlice == null) {
				timeBitmap = null;
				factSet = null;
			} else {
				timeBitmap = timeSlice.getBitmap();
				factSet = timeSlice.getFactsSet();
			}

			for (final GroupResultEntry groupResultEntry : itGroup) {
				final String groupId = createGroupId(groupResultEntry);

				final Bitmap resultBitmap = Bitmap.combineTimeAndFilter(
						indexFactory, timeBitmap, groupResultEntry);

				// create the evaluator
				final ExpressionEvaluator evaluator = new LowExpressionEvaluator(
						index, resultBitmap, factSet, offset + timeSeriesPos);

				// calculate each measure
				if (!fillTimeSeries(groupId, timeSeriesPos, evaluator, result,
						itMeasures)) {
					return null;
				}
			}

			timeSeriesPos++;
		}

		return result;
	}

	/**
	 * Fills the passed {@code TimeSeriesCollection} with the values for the
	 * different measures using the specified evaluator.
	 * 
	 * @param groupId
	 *            the identifier of the current group
	 * @param timeSeriesPos
	 *            the position of the time-series to create the values for
	 * @param evaluator
	 *            the evaluator used to create the results
	 * @param tsc
	 *            the time-series to be filled
	 * @param it
	 *            the iterator used to iterate over the measures
	 * @return {@code true} if the filling of the time-series should be
	 *         processed, if {@code false} is returned any processing is
	 *         cancelled and {@code null} is returned as result of the
	 *         evaluation
	 */
	protected boolean fillTimeSeries(final String groupId,
			final int timeSeriesPos, final ExpressionEvaluator evaluator,
			final TimeSeriesCollection tsc,
			final Iterable<DescriptorMathTree> it) {

		// calculate each measure
		for (final DescriptorMathTree measure : it) {
			final String tsId = createTimeSeriesId(groupId, measure);

			// get or create the series
			final TimeSeries timeSeries = getTimeSeries(tsc, tsId);

			// get the value of the series
			final double value = evaluator.evaluateMeasure(measure);
			timeSeries.setValue(timeSeriesPos, value);
		}

		return true;
	}

	/**
	 * The identifier of the group specified by the entry.
	 * 
	 * @param groupResultEntry
	 *            the entry to create the group for
	 * 
	 * @return the
	 */
	protected String createGroupId(final GroupResultEntry groupResultEntry) {
		if (groupResultEntry == null) {
			return null;
		} else if (groupResultEntry.isGroup()) {
			return groupResultEntry.getGroup();
		} else {
			return null;
		}
	}

	protected TimeSeries getTimeSeries(final TimeSeriesCollection tsc,
			final String tsId) {

		final TimeSeries ts = tsc.getSeries(tsId);
		if (ts == null) {
			return tsc.createSeries(tsId);
		} else {
			return ts;
		}
	}

	protected String createTimeSeriesId(final String groupId,
			final DescriptorMathTree measure) {

		if (groupId == null) {
			return measure.getId();
		} else {
			return groupId + " (" + measure.getId() + ")";
		}
	}

	/**
	 * Determines the bounds specified by the {@code Interval}.
	 * 
	 * @param interval
	 *            the interval to determine the bound for
	 * 
	 * @return the determined bounds
	 */
	protected long[] getBounds(final Interval<?> interval) {
		long[] res;

		if (interval == null) {
			res = new long[] { index.getNormalizedTimeStart(),
					index.getNormalizedTimeEnd() };
		} else {
			res = intervalModel.getTimelineMapper().getBounds(interval);

			if (res == null) {
				throw new ForwardedRuntimeException(
						QueryEvaluationException.class, 1027, interval,
						intervalModel.getTimelineDefinition());
			}
		}

		return res;
	}

	/**
	 * Initializes the {@code TimeSeriesResult} without any {@code TimeSeries}
	 * instances, but with initialized labeling.
	 * 
	 * @param startPoint
	 *            the point to start the time-series at
	 * @param startInclusive
	 *            {@code true} if the start is included, otherwise {@code false}
	 * @param amountOfGranules
	 *            the amount of values within the time-series
	 * 
	 * @return the created {@code TimeSeriesResult}
	 * 
	 * @see TimeSeriesCollection
	 */
	protected TimeSeriesCollection createTimeSeriesResult(
			final Object startPoint, final boolean startInclusive,
			final int amountOfGranules) {

		// create the result
		final TimeSeriesCollection result = new TimeSeriesCollection(
				amountOfGranules, startPoint.getClass(), indexFactory);
		result.setLabels(index, startPoint, startInclusive);

		return result;
	}

	/**
	 * Creates the result of the time-series for the specified {@code members}.
	 * 
	 * @param members
	 *            the members to create the time-series result for
	 * 
	 * @return the created collection of time-series
	 */
	protected TimeSeriesCollection createTimeSeriesResult(
			final Set<TimeLevelMember> members) {
		final TimeSeriesCollection result = new TimeSeriesCollection(
				members.size(), String.class, indexFactory);
		result.setLabels(members);

		return result;
	}

	/**
	 * Combines the bitmaps of the {@code timeSlice}, the {@code filter} and the
	 * specified {@code group}. The method returns {@code null} if the
	 * {@code timeSlice} or it's bitmap is {@code null}.
	 * 
	 * @param timeSlice
	 *            the {@code Slice}
	 * @param filteredValidRecords
	 *            the result of the combination of valid records and the filter
	 *            (see {@link Bitmap#combineBitmaps(Bitmap, IBitmapResult)}.
	 * @param group
	 *            the result of the group
	 * 
	 * @return the combined bitmap, is {@code null} if the {@code timeSlice} was
	 *         {@code null}
	 */
	protected Bitmap combineBitmaps(final SliceWithDescriptors<?> timeSlice,
			final Bitmap filteredValidRecords, final IBitmapResult group) {

		// check if we have no or only a timeSlice
		if (timeSlice == null) {
			return null;
		} else if (filteredValidRecords == null && group == null) {
			return timeSlice.getBitmap();
		}

		// get the bitmap of the timeSlice
		Bitmap resultBitmap = timeSlice.getBitmap();
		if (resultBitmap == null) {
			return null;
		}

		// apply the filter and group to the Bitmap
		if (filteredValidRecords != null && group != null) {
			resultBitmap = resultBitmap.and(filteredValidRecords,
					group.getBitmap());
		} else if (group != null) {
			resultBitmap = resultBitmap.and(group.getBitmap());
		} else if (filteredValidRecords != null) {
			resultBitmap = resultBitmap.and(filteredValidRecords);
		}

		// return the result
		return resultBitmap;
	}
}
