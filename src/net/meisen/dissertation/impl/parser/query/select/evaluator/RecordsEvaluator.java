package net.meisen.dissertation.impl.parser.query.select.evaluator;

import net.meisen.dissertation.impl.parser.query.Interval;
import net.meisen.dissertation.impl.parser.query.select.SelectResult;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.SliceWithDescriptors;

public class RecordsEvaluator {

	private final TidaIndex index;
	private final BaseIndexFactory indexFactory;

	public RecordsEvaluator(final TidaModel model) {
		this.index = model.getIndex();
		this.indexFactory = model.getIndexFactory();
	}

	public Bitmap evaluateInterval(final Interval<?> interval,
			final SelectResult queryResult) {

		// get the valid records
		final Bitmap validBitmap = queryResult.getValidRecords();

		// get the filter
		final DescriptorLogicResult filterResult = queryResult
				.getFilterResult();
		final Bitmap filterBitmap = filterResult == null ? null : filterResult
				.getBitmap();

		// determine the interval
		final SliceWithDescriptors<?>[] timeSlices = getIntervalIndexSlices(interval);

		// combine the slices
		Bitmap bitmap = Bitmap.and(indexFactory, validBitmap, filterBitmap);
		if (timeSlices != null) {
			for (final SliceWithDescriptors<?> timeSlice : timeSlices) {
				final Bitmap timeSliceBitmap = timeSlice.getBitmap();

				// check if there is a bitmap defined
				if (timeSliceBitmap != null) {
					bitmap = Bitmap.and(indexFactory, bitmap, timeSliceBitmap);
				}
			}
		}

		// return the result
		return bitmap;
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
}
