package net.meisen.dissertation.impl.parser.query.select.evaluator;

import java.util.Collection;
import java.util.Iterator;

import net.meisen.dissertation.impl.measures.Count;
import net.meisen.dissertation.impl.parser.query.Interval;
import net.meisen.dissertation.impl.parser.query.select.SelectResult;
import net.meisen.dissertation.impl.parser.query.select.measures.DescriptorMathTree;
import net.meisen.dissertation.impl.time.series.TimeSeries;
import net.meisen.dissertation.impl.time.series.TimeSeriesCollection;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorModelSet;
import net.meisen.dissertation.model.indexes.datarecord.slices.SliceWithDescriptors;
import net.meisen.dissertation.model.time.timeline.TimelineDefinition;

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

	/**
	 * Standard constructor initializing the {@code TimeSeriesEvaluator} for the
	 * specified {@code model}.
	 * 
	 * @param model
	 *            the model the evaluator is used with
	 */
	public TimeSeriesEvaluator(final TidaModel model) {
		this.index = model.getIndex();
		this.indexFactory = model.getIndexFactory();
		this.timeline = model.getIntervalModel().getTimelineDefinition();
	}

	/**
	 * Evaluates the result for the specified {@code interval}.
	 * 
	 * @param interval
	 *            the interval to evaluate the timeseries for
	 * @param measures
	 *            the measures to be evaluated
	 * @param queryResult
	 *            the result of the parsing and interpretation of the select
	 *            statement
	 * 
	 * @return the created {@code TimeSeries}
	 * 
	 * @see TimeSeries
	 * @see SelectResult
	 */
	public TimeSeriesCollection evaluateInterval(final Interval<?> interval,
			final Collection<DescriptorMathTree> measures,
			final SelectResult queryResult) {

		final Bitmap validRecords = queryResult.getValidRecords();
		final DescriptorLogicResult filterResult = queryResult
				.getFilterResult();
		final GroupResult groupResult = queryResult.getGroupResult();

		// determine the interval
		final SliceWithDescriptors<?>[] timeSlices = getIntervalIndexSlices(interval);

		// create the result
		final TimeSeriesCollection result = createTimeSeriesResult(interval,
				timeSlices.length);

		// create the iterables for the groups
		int groupSize;
		final Iterable<GroupResultEntry> itGroup;
		if (groupResult == null || (groupSize = groupResult.size()) == 0) {
			itGroup = noGroupIterable;
			groupSize = 1;
		} else {
			itGroup = groupResult;
		}

		// create the iterables for the measures
		int measureSize = measures.size();
		final Iterable<DescriptorMathTree> itMeasures;
		if (measures == null || (measureSize = measures.size()) == 0) {
			itMeasures = noMeasureIterable;
			measureSize = 1;
		} else {
			itMeasures = measures;
		}

		final int size = groupSize * measureSize;
		final TimeSeries[] ts = new TimeSeries[size];
		final DescriptorMathTree[] dmt = new DescriptorMathTree[size];

		/*
		 * Iterate over each group and measure and create the timeSeries for it.
		 */
		int counter = 0;
		for (final GroupResultEntry groupResultEntry : itGroup) {
			final String groupId = groupResultEntry == null ? null
					: groupResultEntry.getGroup();

			// calculate each measure
			for (final DescriptorMathTree measure : itMeasures) {
				final String timeSeriesId = groupId == null ? measure.getId()
						: groupId + " (" + measure.getId() + ")";
				final TimeSeries timeSeries = result.createSeries(timeSeriesId);

				ts[counter] = timeSeries;
				dmt[counter] = measure;
				counter++;
			}
		}

		// combine the filter with the valid records
		final Bitmap filteredValidRecords = combineBitmaps(validRecords,
				filterResult);

		/*
		 * Iterate over the different slices and calculate each measure.
		 */
		for (final GroupResultEntry groupResultEntry : itGroup) {
			int timeSlicePos = 0;
			for (final SliceWithDescriptors<?> timeSlice : timeSlices) {
				final Bitmap resultBitmap = combineBitmaps(timeSlice,
						filteredValidRecords, groupResultEntry);
				final FactDescriptorModelSet descriptors = timeSlice == null ? null
						: timeSlice.getFactsSet();

				// create the evaluator
				final MathExpressionEvaluator evaluator = new MathExpressionEvaluator(
						index, resultBitmap, descriptors);

				// calculate the measure for each timeSeries
				int i = 0;
				for (final TimeSeries timeSeries : ts) {
					final double value = evaluator.evaluateMeasure(dmt[i]);
					timeSeries.setValue(timeSlicePos, value);

					i++;
				}

				timeSlicePos++;
			}
		}

		return result;
	}

	/**
	 * Gets the {@code Slices} available for the defined {@code interval}.
	 * 
	 * @param interval
	 *            the {@code Slices} available
	 * 
	 * @return the {@code Slices} selected by the {@code interval}
	 */
	protected SliceWithDescriptors<?>[] getIntervalIndexSlices(
			final Interval<?> interval) {
		if (interval == null) {
			return index.getIntervalIndexSlices();
		} else {
			return index.getIntervalIndexSlices(interval.getStart(),
					interval.getEnd(), interval.getOpenType().isInclusive(),
					interval.getCloseType().isInclusive());
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
				amountOfGranules, interval.getType(), indexFactory);
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
	 * Combines the valid records (see {@link TidaModel#getValidRecords()} with
	 * the result of the filter.
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
