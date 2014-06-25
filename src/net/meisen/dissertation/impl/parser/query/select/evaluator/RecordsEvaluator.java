package net.meisen.dissertation.impl.parser.query.select.evaluator;

import net.meisen.dissertation.impl.parser.query.Interval;
import net.meisen.dissertation.impl.parser.query.select.SelectResult;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.SliceWithDescriptors;

/**
 * Evaluator used to evaluate the selected records of a {@code SelectResult}.
 * 
 * @author pmeisen
 * 
 */
public class RecordsEvaluator {

	private final TidaIndex index;
	private final BaseIndexFactory indexFactory;

	/**
	 * Default constructor specifying for which {@code model} the evaluation
	 * takes place.
	 * 
	 * @param model
	 *            the model the evaluation takes place for
	 */
	public RecordsEvaluator(final TidaModel model) {
		this.index = model.getIndex();
		this.indexFactory = model.getIndexFactory();
	}

	/**
	 * Evaluates the result for the specified {@code interval}.
	 * 
	 * @param interval
	 *            the interval to evaluate the records for
	 * @param queryResult
	 *            the result of the parsing and interpretation of the select
	 *            statement
	 * 
	 * @return the selected records
	 * 
	 * @see SelectResultRecords
	 */
	public Bitmap evaluateInterval(final Interval<?> interval,
			final SelectResult queryResult) {

		// determine the interval
		final SliceWithDescriptors<?>[] timeSlices = getIntervalIndexSlices(interval);

		// check the combination
		Bitmap bitmap = null;
		if (timeSlices != null && timeSlices.length >= 0) {
			for (final SliceWithDescriptors<?> timeSlice : timeSlices) {
				final Bitmap timeSliceBitmap = timeSlice.getBitmap();

				// check if there is a bitmap defined
				if (timeSliceBitmap != null) {
					if (bitmap == null) {
						bitmap = timeSliceBitmap;
					} else {
						bitmap = Bitmap.or(indexFactory, bitmap,
								timeSliceBitmap);
					}
				}
			}

			// combine the valid once with it
			final Bitmap validBitmap = queryResult.getValidRecords();
			if (validBitmap != null) {
				bitmap = Bitmap.and(indexFactory, bitmap, validBitmap);
			}

			// combine the filter with it
			final DescriptorLogicResult filterResult = queryResult
					.getFilterResult();
			final Bitmap filterBitmap = filterResult == null ? null
					: filterResult.getBitmap();
			if (filterBitmap != null) {
				bitmap = Bitmap.and(indexFactory, bitmap, filterBitmap);
			}
		}

		// create an empty one if we still don't have any
		if (bitmap == null) {
			bitmap = indexFactory.createBitmap();
		}

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
