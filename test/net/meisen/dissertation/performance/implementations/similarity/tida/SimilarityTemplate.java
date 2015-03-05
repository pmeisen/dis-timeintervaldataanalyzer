package net.meisen.dissertation.performance.implementations.similarity.tida;

import net.meisen.dissertation.impl.parser.query.Interval;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.parser.query.select.SelectResultTimeSeries;
import net.meisen.dissertation.impl.parser.query.select.SelectResultType;
import net.meisen.dissertation.impl.parser.query.select.evaluator.BaseBitmapPreProcessorObservable;
import net.meisen.dissertation.impl.parser.query.select.evaluator.IBitmapPreProcessorObserver;
import net.meisen.dissertation.impl.time.series.TimeSeriesCollection;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.general.genmisc.types.Numbers;

public class SimilarityTemplate implements IBitmapPreProcessorObserver {

	private final long[] bounds;
	private final SelectQuery query;
	private final SelectResultTimeSeries queryResult;
	private final TimeSeriesSimilarityEvaluator evaluator;

	private TimeSeriesCollection measureResult;
	private CountValuesCollection countResult;
	private StructureCollection structureResult;

	private boolean created;

	public SimilarityTemplate(final SelectQuery query,
			final BaseMapper<?> mapper,
			final TimeSeriesSimilarityEvaluator evaluator) {
		if (!SelectResultType.TIMESERIES.equals(query.getResultType())) {
			throw new IllegalStateException("Must be a TimeSeries");
		}

		this.created = false;
		this.evaluator = evaluator;

		// get the bounds
		final Interval<?> interval = query.getInterval();
		this.bounds = mapper.getBounds(interval);

		// generate the timeseries of the template query
		this.queryResult = (SelectResultTimeSeries) evaluator
				.prepareResult(query);

		/*
		 * We need a trigger which is used, if measures are needed we trigger
		 * using the defined query, otherwise we trigger using a count-query.
		 */
		if (evaluator.isCalcMeasure()) {
			this.query = query;
		} else {
			this.query = new SelectQuery();
			this.query.set(query);

			// remove any measure dimension and add the count
			this.query.setMeasureDimension(null);
			this.query
					.setMeasures(TimeSeriesSimilarityEvaluator.countMeasureCollection);
		}
	}

	public void create() {

		if (this.created) {
			// TODO: make it nice
			throw new IllegalStateException("");
		}

		// get the defined similarities needed
		final boolean m = evaluator.isCalcMeasure();
		final boolean c = evaluator.isCalcCount();
		final boolean s = evaluator.isCalcStructure();

		// we only need an observer if the structure or count have to be created
		if (c || s) {
			evaluator.addObserver(this);
		}

		// we need an instance for the structure if needed
		final int size = amountOfTimepoints();
		this.structureResult = s ? new StructureCollection(size) : null;
		this.countResult = c ? new CountValuesCollection(size) : null;

		// trigger the creation and use the result if needed
		final TimeSeriesCollection res = evaluator.evaluateInterval(getQuery(),
				getQueryResult());
		this.measureResult = m ? res : null;

		// finalize the structure result
		if (this.structureResult != null) {
			this.structureResult.finish();
		}

		// clean-up
		this.evaluator.removeObserver(this);

		// finalize
		this.created = true;
	}

	public long[] getBounds() {
		return bounds;
	}

	public SelectQuery getQuery() {
		return query;
	}

	public SelectResultTimeSeries getQueryResult() {
		return queryResult;
	}

	public TimeSeriesCollection getMeasureResult() {
		return measureResult;
	}

	public CountValuesCollection getCountResult() {
		return countResult;
	}

	public StructureCollection getStructureResult() {
		return structureResult;
	}

	public boolean isInCreation() {
		return !created;
	}
	
	@Override
	public String toString() {
		return super.toString();
	}

	@Override
	public boolean preProcessBitmap(
			final BaseBitmapPreProcessorObservable observable,
			final String groupId, final long timepoint, final Bitmap bitmap) {

		if (isInCreation()) {
			final int normalizedTimePoint = Numbers.castToInt(timepoint
					- bounds[0]);

			if (this.countResult != null) {
				this.countResult.set(groupId, normalizedTimePoint, bitmap);
			}

			if (this.structureResult != null) {
				this.structureResult.set(groupId, normalizedTimePoint, bitmap);
			}
		} else {
			// TODO: make it nice
			throw new IllegalStateException("");
		}

		return true;
	}

	public int amountOfTimepoints() {
		return Numbers.castToInt(bounds[1] - bounds[0] + 1);
	}
}
