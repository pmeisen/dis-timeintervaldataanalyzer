package net.meisen.dissertation.impl.parser.query.select.evaluator;

import java.util.Collection;
import java.util.Iterator;

import net.meisen.dissertation.impl.measures.Count;
import net.meisen.dissertation.impl.parser.query.select.Interval;
import net.meisen.dissertation.impl.parser.query.select.measures.DescriptorMathTree;
import net.meisen.dissertation.impl.time.series.TimeSeries;
import net.meisen.dissertation.impl.time.series.TimeSeriesResult;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.SliceWithDescriptors;
import net.meisen.dissertation.model.time.timeline.TimelineDefinition;

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

	public TimeSeriesEvaluator(final TidaModel model) {
		this.index = model.getIndex();
		this.indexFactory = model.getIndexFactory();
		this.timeline = model.getIntervalModel().getTimelineDefinition();
	}

	public TimeSeriesResult evaluateInterval(final Interval interval,
			final Collection<DescriptorMathTree> measures,
			final DescriptorLogicResult filterResult,
			final GroupResult groupResult) {

		// determine the interval
		final SliceWithDescriptors<?>[] timeSlices = getTimeSlices(interval);

		// create the result
		final TimeSeriesResult result = createTimeSeriesResult(interval,
				timeSlices.length);

		// create the iterables for empty results
		final Iterable<GroupResultEntry> itGroup = groupResult == null
				|| groupResult.size() == 0 ? noGroupIterable : groupResult;
		final Iterable<DescriptorMathTree> itMeasures = measures == null
				|| measures.size() == 0 ? noMeasureIterable : measures;

		// iterate over each group
		for (final GroupResultEntry groupResultEntry : itGroup) {
			final String groupId = groupResultEntry == null ? null
					: groupResultEntry.getGroup();

			// iterate over the different slices
			int timeSlicePos = 0;
			for (final SliceWithDescriptors<?> timeSlice : timeSlices) {
				final Bitmap resultBitmap = combineBitmaps(timeSlice,
						filterResult, groupResultEntry);

				// create the evaluator
				final MathExpressionEvaluator evaluator = new MathExpressionEvaluator(
						index, timeSlice, resultBitmap);

				// calculate each measure
				for (final DescriptorMathTree measure : itMeasures) {
					final String timeSeriesId = groupId == null ? measure
							.getId() : groupId + " (" + measure.getId() + ")";

					final TimeSeries timeSeries = result
							.getSeries(timeSeriesId);
					final double value = evaluator.evaluateMeasure(measure);
					timeSeries.setValue(timeSlicePos, value);
				}

				timeSlicePos++;
			}
		}

		return result;
	}

	protected SliceWithDescriptors<?>[] getTimeSlices(final Interval interval) {
		if (interval == null) {
			return index.getIntervalIndexSlices();
		} else {
			return index.getIntervalIndexSlices(interval.getStart(),
					interval.getEnd(), interval.getOpenType().isInclusive(),
					interval.getCloseType().isInclusive());
		}
	}

	protected TimeSeriesResult createTimeSeriesResult(final Interval interval,
			final int amountOfGranules) {

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
		final TimeSeriesResult result = new TimeSeriesResult(amountOfGranules,
				indexFactory);
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

	protected Bitmap combineBitmaps(final SliceWithDescriptors<?> timeSlice,
			final IBitmapResult filter, final IBitmapResult group) {

		// check if we have no or only a timeSlice
		if (timeSlice == null) {
			return null;
		} else if (filter == null && group == null) {
			return timeSlice.getBitmap();
		}

		// get the bitmap of the timeSlice
		Bitmap resultBitmap = timeSlice.getBitmap();
		if (resultBitmap == null) {
			return null;
		}

		// apply the filter and group to the Bitmap
		if (filter != null && group != null) {
			resultBitmap = resultBitmap.and(filter.getBitmap(),
					group.getBitmap());
		} else if (group != null) {
			resultBitmap = resultBitmap.and(group.getBitmap());
		} else if (filter != null) {
			resultBitmap = resultBitmap.and(filter.getBitmap());
		}

		// return the result
		return resultBitmap;
	}
}
