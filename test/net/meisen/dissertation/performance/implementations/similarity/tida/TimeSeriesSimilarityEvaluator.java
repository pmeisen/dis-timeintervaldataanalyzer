package net.meisen.dissertation.performance.implementations.similarity.tida;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.parser.query.select.SelectResultTimeSeries;
import net.meisen.dissertation.impl.parser.query.select.SelectResultType;
import net.meisen.dissertation.impl.parser.query.select.evaluator.BaseBitmapPreProcessorObservable;
import net.meisen.dissertation.impl.parser.query.select.evaluator.ExpressionEvaluator;
import net.meisen.dissertation.impl.parser.query.select.evaluator.IBitmapPreProcessorObserver;
import net.meisen.dissertation.impl.parser.query.select.evaluator.SelectEvaluator;
import net.meisen.dissertation.impl.parser.query.select.evaluator.TimeSeriesEvaluator;
import net.meisen.dissertation.impl.parser.query.select.measures.DescriptorMathTree;
import net.meisen.dissertation.impl.time.series.TimeSeries;
import net.meisen.dissertation.impl.time.series.TimeSeriesCollection;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.dissertation.performance.implementations.similarity.tida.TemporalRelation.Relation;
import net.meisen.general.genmisc.types.Numbers;

public class TimeSeriesSimilarityEvaluator extends TimeSeriesEvaluator
		implements IBitmapPreProcessorObserver {
	protected final static List<DescriptorMathTree> countMeasureCollection;
	static {
		countMeasureCollection = new ArrayList<DescriptorMathTree>(1);
		countMeasureCollection.add(countMeasure);
	}

	private final SelectEvaluator evaluator;

	private SimilarityTemplate template;
	private long[] currentPartition;
	private TimeSeriesSimilarityCollection currentTssc;

	private double timePointThresholdMeasureDistance;
	private double timePointThresholdCountDistance;
	private double timePointThresholdStructureDistance;
	private double thresholdDistance;
	private double currentThresholdDistance;

	private boolean calcMeasure;
	private boolean calcCount;
	private boolean calcStructure;

	public TimeSeriesSimilarityEvaluator(final TidaModel model) {
		super(model);

		this.evaluator = new SelectEvaluator(model);
		this.timePointThresholdMeasureDistance = Double.MAX_VALUE;
		this.timePointThresholdCountDistance = Double.MAX_VALUE;
		this.timePointThresholdStructureDistance = Double.MAX_VALUE;
		this.thresholdDistance = Double.MAX_VALUE;

		// initialize state
		this.setSimilarity(true, false, false);
		this.reset();
	}

	public void setSimilarity(final boolean measure, final boolean count,
			final boolean structure) {
		this.calcMeasure = measure;
		this.calcCount = count;
		this.calcStructure = structure;

	}

	protected boolean isCalcMeasure() {
		return calcMeasure;
	}

	protected boolean isCalcCount() {
		return calcCount;
	}

	protected boolean isCalcStructure() {
		return calcStructure;
	}

	@Override
	protected TimeSeriesCollection createTimeSeriesResult(final long[] bounds) {
		final int size = Numbers.castToInt(bounds[1] - bounds[0] + 1);

		if (template.isInCreation()) {
			return super.createTimeSeriesResult(bounds);
		} else {
			currentTssc = new TimeSeriesSimilarityCollection(size, size,
					intervalModel.getTimelineMapper().getMappedType(),
					indexFactory);
			return currentTssc;
		}
	}

	@Override
	protected TimeSeriesCollection createTimeSeriesResult(
			final Set<TimeLevelMember> members) {
		final int size = members.size();

		if (template.isInCreation()) {
			return super.createTimeSeriesResult(members);
		} else {
			currentTssc = new TimeSeriesSimilarityCollection(size,
					template.amountOfTimepoints(), String.class, indexFactory);
			return currentTssc;
		}
	}

	@Override
	protected boolean fillTimeSeries(final String groupId,
			final int timeSeriesPos, final long size,
			final ExpressionEvaluator evaluator,
			final TimeSeriesCollection tsc,
			final Iterable<DescriptorMathTree> it) {

		if (template.isInCreation()) {
			return super.fillTimeSeries(groupId, timeSeriesPos, size,
					evaluator, tsc, it);
		} else {
			for (final DescriptorMathTree measure : it) {

				// get the value and check if it's useful
				final double value = evaluator.evaluateMeasure(measure);
				if (evaluator.isCancelled(value)) {
					return false;
				}

				// get the identifier of the time-series
				final String tsId = createTimeSeriesId(groupId, measure);
				if (!calculateMeasure(tsId, timeSeriesPos, size, value)) {
					return false;
				}

				// get or create the series
				final TimeSeries timeSeries = getTimeSeries(currentTssc, tsId);
				timeSeries.setValue(timeSeriesPos, value);
			}
		}

		return true;
	}

	@Override
	public boolean preProcessBitmap(
			final BaseBitmapPreProcessorObservable observable,
			final String groupId, final long timepoint, final Bitmap bitmap) {

		if (this.template.isInCreation()) {
			// TODO: make nice
			throw new IllegalStateException(
					"Should not be registered as Observer");
		} else {
			final int normalizedTimePoint = Numbers.castToInt(timepoint
					- this.currentPartition[0]);
			if (currentTssc.isProcessed(groupId, normalizedTimePoint)) {
				return true;
			}

			// calculate the count
			if (!calculateCount(groupId, normalizedTimePoint, bitmap)) {
				return false;
			}

			if (!calculateStructure(groupId, normalizedTimePoint, bitmap)) {
				return false;
			}

			return true;
		}
	}

	protected boolean calculateMeasure(final String tsId, final int timepoint,
			final long size, final double measure) {

		// get the template instance and validate the measure
		if (isCalcMeasure()) {
			final TimeSeries templateSeries = getTimeSeries(
					template.getMeasureResult(), tsId);
			final double templateValue = templateSeries.getValue(timepoint);

			// calculate the distance
			final double dist = Math.abs(templateValue - measure);
			if (dist >= this.timePointThresholdMeasureDistance) {
				return false;
			}

			/*
			 * TODO: currently only weighted by amount of timepoints, should be
			 * weighted according to how much measure unit is one count, i.e.
			 * count per measure unit
			 */
			// get the total distance and validate it
			final double weightedDist = size * dist;
			final double totalDist = weightedDist
					+ currentTssc.getTotalDistance();
			if (totalDist >= this.currentThresholdDistance) {
				return false;
			}

			// get the value of the series
			currentTssc.setMeasureDistance(weightedDist
					+ currentTssc.getMeasureDistance());
		}

		return true;
	}

	protected boolean calculateCount(final String groupId, final int timepoint,
			final Bitmap bitmap) {

		// check the count
		if (isCalcCount()) {

			final CountValuesCollection countResult = this.template
					.getCountResult();

			final int templateCount = countResult.get(groupId, timepoint);
			final int currentCount = countResult.calcCount(bitmap);

			// calculate the distance
			final int dist = Math.abs(templateCount - currentCount);
			if (dist >= this.timePointThresholdCountDistance) {
				return false;
			}

			// get the total distance and validate it
			final double totalDist = dist + currentTssc.getTotalDistance();
			if (totalDist >= this.currentThresholdDistance) {
				return false;
			}

			currentTssc.setCountDistance(dist + currentTssc.getCountDistance());
			currentTssc.setCount(groupId, timepoint, currentCount);
		}

		return true;
	}

	protected boolean calculateStructure(final String groupId, final int pos,
			final Bitmap bitmap) {

		if (isCalcStructure()) {
			final StructureCollection structureResult = this.template
					.getStructureResult();

			final int[] currentValues = currentTssc.setStructure(groupId, pos,
					bitmap);
			final int[] templateValues = structureResult.get(groupId, pos);

			if (!calculateStructure(templateValues, currentValues)) {
				return false;
			}
		}

		return true;
	}

	private boolean calculateStructure(final int[] templateVals,
			final int[] currentVals) {

		double dist = 0.0;
		double weightedDist = 0.0;
		if (templateVals == null && currentVals == null) {
			// do nothing 0.0 is the result
		} else if (templateVals == null || currentVals == null) {
			final int[] nonNull = templateVals == null ? currentVals
					: templateVals;
			final int length = nonNull.length;
			final int count = nonNull[0];

			for (int i = 1; i < length; i++) {
				final int val = nonNull[i];

				dist += val;
			}

			// SUM(VAL) * (COUNT / SUM(VAL))
			weightedDist = count;
		} else {
			final int length = templateVals == null ? currentVals.length
					: templateVals.length;

			/*
			 * Use Manhattan distance to calculate difference between the
			 * relations.
			 */
			final int tCount = templateVals[0];
			final int cCount = currentVals[0];
			final int count = Math.max(tCount, cCount);

			int sum = 0;
			for (int i = 1; i < length; i++) {
				final int tVal = templateVals[i];
				final int cVal = currentVals[i];

				dist += Math.abs(tVal - cVal);
				sum += Math.max(tVal, cVal);
			}

			weightedDist = sum == 0.0 ? 0.0 : dist * ((double) count / sum);
		}

		if (dist >= this.timePointThresholdStructureDistance) {
			return false;
		}

		// get the total distance and validate it
		final double totalDist = weightedDist + currentTssc.getTotalDistance();
		if (totalDist >= this.currentThresholdDistance) {
			return false;
		}

		// just set the new distance
		currentTssc.setStructureDistance(weightedDist
				+ currentTssc.getStructureDistance());

		return true;
	}

	private boolean finish() {
		currentTssc.finish();

		// if we have to calculate the structure do it for the last timepoint
		if (isCalcStructure()) {
			final StructureCollection structureResult = this.template
					.getStructureResult();
			final Set<String> groupIds = structureResult.getGroups();

			final int last = structureResult.getMaxPosition();
			for (final String groupId : groupIds) {
				final int[] templateValues = structureResult.get(groupId, last);
				final int[] currentValues = currentTssc.getStructure(groupId,
						last);

				if (!calculateStructure(templateValues, currentValues)) {
					return false;
				}
			}
		}

		return true;
	}

	public List<TimeSeriesSimilarityCollection> evaluateSimilarity(
			final SelectQuery query, final int k) {

		// TODO: make it nice, make sure it's not currently processing
		if (template != null) {
			throw new IllegalStateException("Cannot run several times");
		} else if (!SelectResultType.TIMESERIES.equals(query.getResultType())) {
			throw new IllegalStateException("Must be a TimeSeries");
		}

		// get the bounds
		final BaseMapper<?> mapper = intervalModel.getTimelineMapper();
		this.template = new SimilarityTemplate(query, mapper, this);
		this.template.create();
		if (this.template.isInCreation()) {
			// TODO: make nice
			throw new IllegalStateException("Should be finished");
		}

		// add this as observer
		this.addObserver(this);

		// get the iterator to iterate over the different parts of the time-line
		final long[] templateBounds = template.getBounds();
		final Iterator<long[]> it = mapper
				.createTimelinePartitionIterator(templateBounds);

		// evaluate each partition
		final List<TimeSeriesSimilarityCollection> results = new ArrayList<TimeSeriesSimilarityCollection>();
		int counter = 0;

		while (it.hasNext()) {
			final long[] partition = it.next();

			// skip the requested one
			if (templateBounds[0] == partition[0]) {
				continue;
			}

			// evaluate the result for the partition
			this.currentPartition = partition;

			// if we have an result finish it and add it if everything is fine
			if (evaluateInterval(this.template, partition) != null && finish()) {
				counter++;

				if (counter < k) {
					results.add(currentTssc);
				} else {
					results.add(currentTssc);
					Collections.sort(results);

					final TimeSeriesSimilarityCollection simRes = results
							.get(k - 1);
					this.currentThresholdDistance = simRes.getTotalDistance();

					if (counter > k) {
						results.remove(k);
					}
				}
			}
		}

		// sort if not sorted yet
		if (counter < k) {
			Collections.sort(results);
		}

		// reset the evaluator
		reset();

		// return the results
		return results;
	}

	public TimeSeriesSimilarityCollection evaluateInterval(
			final SimilarityTemplate template, final long[] partition) {
		return (TimeSeriesSimilarityCollection) evaluateInterval(
				template.getQuery(), partition, template.getQueryResult());
	}

	protected void reset() {

		// remove the observers
		this.removeObserver(this);
		this.removeObserver(this.template);

		// reset the values
		this.template = null;
		this.currentPartition = null;
		this.currentTssc = null;
		this.currentThresholdDistance = this.thresholdDistance;
	}

	public SelectResultTimeSeries prepareResult(final SelectQuery query) {
		return (SelectResultTimeSeries) evaluator.prepareResult(query);
	}

	public double getTimePointThresholdMeasureDistance() {
		return timePointThresholdMeasureDistance;
	}

	public void setTimePointThresholdMeasureDistance(
			final double timePointThresholdMeasureDistance) {
		this.timePointThresholdMeasureDistance = timePointThresholdMeasureDistance;
	}

	public double getTimePointThresholdStructureDistance() {
		return timePointThresholdStructureDistance;
	}

	public void setTimePointThresholdStructureDistance(
			final double timePointThresholdStructureDistance) {
		this.timePointThresholdStructureDistance = timePointThresholdStructureDistance;
	}

	public double getTimePointThresholdCountDistance() {
		return timePointThresholdCountDistance;
	}

	public void setTimePointThresholdCountDistance(
			final double timePointThresholdCountDistance) {
		this.timePointThresholdCountDistance = timePointThresholdCountDistance;
	}
}
