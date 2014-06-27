package net.meisen.dissertation.impl.parser.query.select.evaluator;

import net.meisen.dissertation.impl.parser.query.Interval;
import net.meisen.dissertation.impl.parser.query.select.IntervalRelation;
import net.meisen.dissertation.impl.parser.query.select.SelectResult;
import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;

/**
 * Evaluator used to evaluate the selected records of a {@code SelectResult}.
 * 
 * @author pmeisen
 * 
 */
public class RecordsEvaluator {

	private final TidaIndex index;
	private final BaseIndexFactory indexFactory;
	private final IntervalModel intervalModel;

	/**
	 * Default constructor specifying for which {@code model} the evaluation
	 * takes place.
	 * 
	 * @param model
	 *            the model the evaluation takes place for
	 */
	public RecordsEvaluator(final TidaModel model) {
		this.intervalModel = model.getIntervalModel();
		this.index = model.getIndex();
		this.indexFactory = model.getIndexFactory();
	}

	/**
	 * Evaluates the result for the specified {@code interval}.
	 * 
	 * @param interval
	 *            the interval to evaluate the records for
	 * @param relation
	 *            the {@code IntervalRelation} to determine the result for
	 * @param queryResult
	 *            the result of the parsing and interpretation of the select
	 *            statement
	 * 
	 * @return the selected records
	 * 
	 * @see SelectResultRecords
	 */
	public Bitmap evaluateInterval(final Interval<?> interval,
			final IntervalRelation relation, final SelectResult queryResult) {

		// get the relation or use a default one if none is defined
		final IntervalRelation intervalRelation = relation == null ? IntervalRelation.WITHIN
				: relation;

		// the result bitmap
		Bitmap bitmap = null;

		// combine the valid once with it
		final Bitmap validBitmap = queryResult.getValidRecords();
		bitmap = combine(bitmap, validBitmap);

		// combine the time with it
		if (bitmap == null || bitmap.isBitSet()) {
			final Bitmap timeBitmap = intervalRelation.determine(intervalModel,
					index, interval);
			bitmap = combine(bitmap, timeBitmap);
		}

		// combine the filter with it
		if (bitmap == null || bitmap.isBitSet()) {
			final DescriptorLogicResult filterResult = queryResult
					.getFilterResult();
			final Bitmap filterBitmap = filterResult == null ? null
					: filterResult.getBitmap();
			bitmap = combine(bitmap, filterBitmap);
		}

		// if we don't have any result return null
		if (bitmap == null) {
			bitmap = indexFactory.createBitmap();
		}

		return bitmap;
	}

	/**
	 * Helper method to and-combine the {@code resBitmap} and the
	 * {@code andBitmap}.
	 * 
	 * @param resBitmap
	 *            the result bitmap, can be {@code null} if so it is ignored in
	 *            combination
	 * @param andBitmap
	 *            the and-bitmap to be and-combined, can be {@code null} if so
	 *            it is ignored in combination
	 * 
	 * @return the combination of the {@code resBitmap} and the
	 *         {@code andBitmap}
	 */
	protected Bitmap combine(final Bitmap resBitmap, final Bitmap andBitmap) {
		if (andBitmap == null) {
			return resBitmap;
		} else if (resBitmap == null) {
			return andBitmap;
		} else {
			return Bitmap.and(indexFactory, resBitmap, andBitmap);
		}
	}
}
