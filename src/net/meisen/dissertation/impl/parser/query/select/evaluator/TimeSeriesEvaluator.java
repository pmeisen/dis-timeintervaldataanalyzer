package net.meisen.dissertation.impl.parser.query.select.evaluator;

import net.meisen.dissertation.impl.parser.query.select.Interval;
import net.meisen.dissertation.impl.time.series.TimeSeriesResult;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.IndexDimensionSlice;
import net.meisen.dissertation.model.time.timeline.TimelineDefinition;

public class TimeSeriesEvaluator {
	private final TidaIndex index;
	private final BaseIndexFactory indexFactory;
	private final TimelineDefinition timeline;

	public TimeSeriesEvaluator(final TidaModel model) {
		index = model.getIndex();
		indexFactory = model.getIndexFactory();
		timeline = model.getIntervalModel().getTimelineDefinition();
	}

	public TimeSeriesResult evaluateInterval(final Interval interval,
			final Bitmap filter) {

		// determine the interval
		final boolean startInclusive;
		final boolean endInclusive;
		final IndexDimensionSlice<?>[] timeSlices;
		final Object startPoint;

		if (interval == null) {
			startInclusive = true;
			endInclusive = true;
			timeSlices = index.getIntervalIndexSlices();
			startPoint = timeline.getStart();
		} else {
			startInclusive = interval.getOpenType().isInclusive();
			endInclusive = interval.getCloseType().isInclusive();
			timeSlices = index.getIntervalIndexSlices(interval.getStart(),
					interval.getEnd(), startInclusive, endInclusive);
			startPoint = interval.getStart();
		}

		// create the result
		final TimeSeriesResult result = new TimeSeriesResult(timeSlices.length,
				indexFactory);

		// iterate over the different slices
		int pos = 0;
		for (final IndexDimensionSlice<?> timeSlice : timeSlices) {

			// get the result
			final Bitmap resBitmap;
			if (timeSlice == null) {
				resBitmap = null;
			} else if (filter == null) {
				resBitmap = timeSlice.getBitmap();
			} else {
				resBitmap = filter.and(timeSlice.getBitmap());
			}

			// create a label for the instance
			final Object labelValue = index.getTimePointValue(startPoint,
					startInclusive, pos);
			final String label = index.getTimePointLabel(labelValue);
			result.setLabel(pos, label, labelValue);

			// use the result to calculate the different timeSeries
			if (resBitmap == null) {
				result.setValue(pos, "COUNT", 0);
			} else {
				result.setValue(pos, "COUNT", resBitmap.determineCardinality());
			}

			pos++;
		}

		return result;
	}
}
