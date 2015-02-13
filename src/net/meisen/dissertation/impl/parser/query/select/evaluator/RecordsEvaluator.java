package net.meisen.dissertation.impl.parser.query.select.evaluator;

import net.meisen.dissertation.impl.parser.query.Interval;
import net.meisen.dissertation.impl.parser.query.select.IntervalRelation;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.parser.query.select.SelectResultRecords;
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
	 * @param query
	 *            the {@code SelectQuery} to evaluate
	 * @param filteredBitmap
	 *            the bitmap defining all the filtered bitmaps
	 * 
	 * @return the selected records
	 * 
	 * @see SelectResultRecords
	 */
	public Bitmap evaluateInterval(final SelectQuery query,
			final Bitmap filteredBitmap) {
		final Interval<?> interval = query.getInterval();
		final IntervalRelation relation = query.getIntervalRelation();

		// get the relation or use a default one if none is defined
		final IntervalRelation intervalRelation = relation == null ? IntervalRelation.WITHIN
				: relation;

		// the result bitmap
		final Bitmap result;

		// check if there is a limit
		if (!query.hasLimit() || query.getLimit() != 0) {

			// combine the time with it
			if (filteredBitmap == null || filteredBitmap.isBitSet()) {
				final Bitmap timeBitmap = intervalRelation.determine(
						intervalModel, index, interval);
				result = combine(filteredBitmap, timeBitmap);
			} else {
				result = null;
			}
		} else {
			result = null;
		}

		// if we don't have any result return null
		if (result == null) {
			return indexFactory.createBitmap();
		} else {
			return result;
		}
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
