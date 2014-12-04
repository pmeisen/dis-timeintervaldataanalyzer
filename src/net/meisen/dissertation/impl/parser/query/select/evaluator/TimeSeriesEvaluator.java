package net.meisen.dissertation.impl.parser.query.select.evaluator;

import java.util.Collection;
import java.util.Iterator;
import java.util.Set;

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
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.dissertation.model.dimensions.TimeMemberRange;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorModelSet;
import net.meisen.dissertation.model.indexes.datarecord.slices.IBitmapContainer;
import net.meisen.dissertation.model.indexes.datarecord.slices.SliceWithDescriptors;
import net.meisen.dissertation.model.measures.IDimAggregationFunction;
import net.meisen.dissertation.model.measures.IMathAggregationFunction;
import net.meisen.dissertation.model.time.timeline.TimelineDefinition;
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

	private final TidaIndex index;
	private final BaseIndexFactory indexFactory;
	private final TimelineDefinition timeline;
	private final DimensionModel dimModel;

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
		this.timeline = model.getIntervalModel().getTimelineDefinition();
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

		final Bitmap validRecords = queryResult.getValidRecords();
		final DescriptorLogicResult filterResult = queryResult
				.getFilterResult();
		final GroupResult groupResult = queryResult.getGroupResult();

		// combine the filter with the valid records
		final Bitmap filteredValidRecords = combineBitmaps(validRecords,
				filterResult);

		// depending on the dimensions we have to choose what to do
		if (query.getMeasureDimension() == null) {
			return evaluateLow(query, filteredValidRecords, groupResult);
		} else if (!query.usesFunction(IMathAggregationFunction.class)) {
			return evaluateDim(query, filteredValidRecords, groupResult);
		} else if (!query.usesFunction(IDimAggregationFunction.class)) {
			return evaluateMath(query, filteredValidRecords, groupResult);
		} else {
			return evaluateMixed(query, filteredValidRecords, groupResult);
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
				combinedBitmap = orCombine(slice, combinedBitmap);
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
	 * @param filteredValidRecords
	 *            the bitmap of the filtered records
	 * @param groupResult
	 *            the group result
	 * 
	 * @return the created result
	 */
	protected TimeSeriesCollection evaluateMixed(final SelectQuery query,
			final Bitmap filteredValidRecords, final GroupResult groupResult) {

		// get some stuff we need
		final Interval<?> interval = query.getInterval();
		final Iterable<DescriptorMathTree> itMeasures = it(query.getMeasures());
		final Iterable<GroupResultEntry> itGroup = it(groupResult);

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

			// now we have the bitmap for the specified range, apply the filter
			final Bitmap filteredCombinedBitmap = combineBitmaps(
					filteredValidRecords, combined.getBitmap());

			for (final GroupResultEntry groupResultEntry : itGroup) {
				final String groupId = groupResultEntry == null ? null
						: groupResultEntry.getGroup();
				final Bitmap groupedFilteredBitmap = combineBitmaps(
						filteredValidRecords, groupResultEntry);
				final Bitmap groupedFilteredCombinedBitmap = combineBitmaps(
						filteredCombinedBitmap, groupResultEntry);

				// create the evaluator
				final ExpressionEvaluator evaluator = new MixedExpressionEvaluator(
						index, bounds, member.getRanges(),
						groupedFilteredBitmap, groupedFilteredCombinedBitmap,
						combined.getFacts());

				// calculate each measure
				fillTimeSeries(groupId, timeSeriesPos, evaluator, result,
						itMeasures);
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
	 * @param filteredValidRecords
	 *            the bitmap of the filtered records
	 * @param groupResult
	 *            the group result
	 * 
	 * @return the created result
	 */
	protected TimeSeriesCollection evaluateMath(final SelectQuery query,
			final Bitmap filteredValidRecords, final GroupResult groupResult) {

		// get some stuff we need
		final Interval<?> interval = query.getInterval();
		final Iterable<DescriptorMathTree> itMeasures = it(query.getMeasures());
		final Iterable<GroupResultEntry> itGroup = it(groupResult);

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
				final String groupId = groupResultEntry == null ? null
						: groupResultEntry.getGroup();
				final Bitmap resultBitmap = combineBitmaps(
						filteredValidRecords, groupResultEntry);

				// create the evaluator
				final ExpressionEvaluator evaluator = new MathExpressionEvaluator(
						index, bounds, member.getRanges(), resultBitmap);

				// calculate each measure
				fillTimeSeries(groupId, timeSeriesPos, evaluator, result,
						itMeasures);
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
	 * @param filteredValidRecords
	 *            the bitmap of the filtered records
	 * @param groupResult
	 *            the group result
	 * 
	 * @return the created result
	 */
	protected TimeSeriesCollection evaluateDim(final SelectQuery query,
			final Bitmap filteredValidRecords, final GroupResult groupResult) {

		// get some stuff we need
		final Interval<?> interval = query.getInterval();
		final Iterable<DescriptorMathTree> itMeasures = it(query.getMeasures());
		final Iterable<GroupResultEntry> itGroup = it(groupResult);

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

			// now we have the bitmap for the specified range, apply the filter
			final Bitmap filteredCombinedBitmap = combineBitmaps(
					filteredValidRecords, combined.getBitmap());

			for (final GroupResultEntry groupResultEntry : itGroup) {
				final String groupId = groupResultEntry == null ? null
						: groupResultEntry.getGroup();

				// combine the bitmaps: filter, member, valid with the group
				final Bitmap groupedFilteredCombinedBitmap = combineBitmaps(
						filteredCombinedBitmap, groupResultEntry);

				// create the evaluator
				final ExpressionEvaluator evaluator = new DimExpressionEvaluator(
						index, groupedFilteredCombinedBitmap,
						combined.getFacts());

				// calculate each measure
				fillTimeSeries(groupId, timeSeriesPos, evaluator, result,
						itMeasures);
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
	 * @param filteredValidRecords
	 *            the bitmap of the filtered records
	 * @param groupResult
	 *            the group result
	 * 
	 * @return the created result
	 */
	protected TimeSeriesCollection evaluateLow(final SelectQuery query,
			final Bitmap filteredValidRecords, final GroupResult groupResult) {

		// get some stuff we need
		final Interval<?> interval = query.getInterval();
		final Iterable<DescriptorMathTree> itMeasures = it(query.getMeasures());
		final Iterable<GroupResultEntry> itGroup = it(groupResult);

		// determine the window and get the slices needed
		final long[] bounds = getBounds(interval);
		final SliceWithDescriptors<?>[] timeSlices = index
				.getIntervalIndexSlices(bounds[0], bounds[1]);

		// create the result
		final TimeSeriesCollection result = createTimeSeriesResult(interval,
				timeSlices.length);

		/*
		 * Iterate over the different slices and calculate each measure.
		 */
		int offset = Numbers.castToInt(bounds[0]);
		int timeSeriesPos = 0;
		for (final SliceWithDescriptors<?> timeSlice : timeSlices) {
			final FactDescriptorModelSet factSet = timeSlice == null ? null
					: timeSlice.getFactsSet();

			for (final GroupResultEntry groupResultEntry : itGroup) {
				final String groupId = groupResultEntry == null ? null
						: groupResultEntry.getGroup();

				final Bitmap resultBitmap = combineBitmaps(timeSlice,
						filteredValidRecords, groupResultEntry);

				// create the evaluator
				final ExpressionEvaluator evaluator = new LowExpressionEvaluator(
						index, resultBitmap, factSet, offset + timeSeriesPos);

				// calculate each measure
				fillTimeSeries(groupId, timeSeriesPos, evaluator, result,
						itMeasures);
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
	 * @param timeSeriesCollection
	 *            the time-series to be filled
	 * @param it
	 *            the iterator used to iterate over the measures
	 */
	protected void fillTimeSeries(final String groupId,
			final int timeSeriesPos, final ExpressionEvaluator evaluator,
			final TimeSeriesCollection timeSeriesCollection,
			final Iterable<DescriptorMathTree> it) {

		// calculate each measure
		for (final DescriptorMathTree measure : it) {
			final String timeSeriesId = groupId == null ? measure.getId()
					: groupId + " (" + measure.getId() + ")";

			// get or create the series
			TimeSeries timeSeries = timeSeriesCollection
					.getSeries(timeSeriesId);
			if (timeSeries == null) {
				timeSeries = timeSeriesCollection.createSeries(timeSeriesId);
			}

			// get the value of the series
			final double value = evaluator.evaluateMeasure(measure);
			timeSeries.setValue(timeSeriesPos, value);
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
		if (interval == null) {
			return new long[] { index.getNormalizedTimeStart(),
					index.getNormalizedTimeEnd() };
		} else {
			return index.getBounds(interval.getStart(), interval.getEnd(),
					interval.getOpenType().isInclusive(), interval
							.getCloseType().isInclusive());
		}
	}

	/**
	 * Initializes the {@code TimeSeriesResult} without any {@code TimeSeries}
	 * instances, but with initialized labeling.
	 * 
	 * @param interval
	 *            the interval the {@code TimeSeries} should be based on
	 * @param amountOfGranules
	 *            the amount of granules defined for the {@code TimeSeries}
	 * 
	 * @return the created {@code TimeSeriesResult}
	 * 
	 * @see TimeSeriesCollection
	 */
	protected TimeSeriesCollection createTimeSeriesResult(
			final Interval<?> interval, final int amountOfGranules) {

		// determine the boundaries
		final Object startPoint;
		final boolean startInclusive;
		if (interval == null) {
			startInclusive = true;
			startPoint = timeline.getStart();
		} else {
			startInclusive = interval.getOpenType().isInclusive();
			startPoint = interval.getStart();
		}

		// create the result
		final TimeSeriesCollection result = new TimeSeriesCollection(
				amountOfGranules, startPoint.getClass(), indexFactory);
		for (int i = 0; i < amountOfGranules; i++) {

			// create a label for the timeSlice
			final Object timeSliceLabel = index.getTimePointValue(startPoint,
					startInclusive, i);
			final String formattedTimeSliceLabel = index
					.getTimePointLabel(timeSliceLabel);

			// set the label formatted and the real object
			result.setLabel(i, formattedTimeSliceLabel, timeSliceLabel);
		}

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
		final int amountOfGranules = members.size();
		final TimeSeriesCollection result = new TimeSeriesCollection(
				amountOfGranules, String.class, indexFactory);

		int i = 0;
		for (final TimeLevelMember member : members) {
			result.setLabel(i, member.getName(), member.getId());

			i++;
		}

		return result;
	}

	/**
	 * Combines the valid records (see {@link TidaModel#getValidRecords()} with
	 * the result of the filter. A {@code null} value is handled as a
	 * full-bitmap, e.g. all values are set to one. If both bitmaps are
	 * {@code null}, {@code null} is returned.
	 * 
	 * @param validRecords
	 *            the valid records
	 * @param filter
	 *            the filter
	 * 
	 * @return the result of the combination of both
	 */
	protected Bitmap combineBitmaps(final Bitmap validRecords,
			final IBitmapResult filter) {
		if (filter == null) {
			return validRecords;
		} else if (validRecords == null) {
			return filter.getBitmap();
		} else {
			return validRecords.and(filter.getBitmap());
		}
	}

	/**
	 * Combines the two bitmaps with each-other using an and-operation. A
	 * {@code null} bitmap is handled as a full-bitmap, e.g. all values are set
	 * to one. If both bitmaps are {@code null}, {@code null} is returned.
	 * 
	 * @param bitmap1
	 *            the first bitmap
	 * @param bitmap2
	 *            the second bitmap
	 * 
	 * @return the result of the combination
	 */
	protected Bitmap combineBitmaps(final Bitmap bitmap1, final Bitmap bitmap2) {
		if (bitmap1 == null) {
			return bitmap2;
		} else if (bitmap2 == null) {
			return bitmap1;
		} else {
			return bitmap1.and(bitmap2);
		}
	}

	/**
	 * Combines the container and the bitmap with each-other using an
	 * or-operation.
	 * 
	 * @param container
	 *            the {@code BitmapContainer}
	 * @param bitmap
	 *            the bitmap
	 * 
	 * @return the result of or-combination
	 */
	protected Bitmap orCombine(final IBitmapContainer container,
			final Bitmap bitmap) {
		if (container == null) {
			return bitmap;
		} else if (bitmap == null) {
			return container.getBitmap();
		} else {
			return bitmap.or(container.getBitmap());
		}
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
	 *            (see {@link #combineBitmaps(Bitmap, IBitmapResult)}.
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
