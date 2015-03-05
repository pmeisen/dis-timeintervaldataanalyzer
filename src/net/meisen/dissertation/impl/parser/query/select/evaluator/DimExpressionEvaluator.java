package net.meisen.dissertation.impl.parser.query.select.evaluator;

import java.util.List;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.impl.measures.MapFactsDescriptorBased;
import net.meisen.dissertation.model.dimensions.TimeMemberRange;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorModelSet;
import net.meisen.dissertation.model.indexes.datarecord.slices.SliceWithDescriptors;
import net.meisen.dissertation.model.measures.IAggregationFunction;
import net.meisen.dissertation.model.measures.IDimAggregationFunction;
import net.meisen.dissertation.model.measures.IFactsHolder;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * Evaluator used to evaluate a measure based on {@code IDimAggregationFunction}
 * .
 * 
 * @author pmeisen
 * 
 */
public class DimExpressionEvaluator extends ExpressionEvaluator {
	private final Bitmap resultBitmap;
	private final FactDescriptorModelSet facts;
	private final long[] bounds;
	private final List<TimeMemberRange> ranges;
	private final Bitmap filterBitmap;

	/**
	 * Default constructor to create a {@code DimExpressionEvaluator} for a low
	 * granularity-aggregation.
	 * 
	 * @param index
	 *            the index to retrieve the data from
	 * @param groupId
	 *            the group the expression is evaluated for
	 * @param bounds
	 *            the bounds defined generally
	 * @param ranges
	 *            the ranges to evaluate
	 * @param filterBitmap
	 *            the resulting bitmap defining which records are filtered using
	 *            filtering, grouping and validation
	 * @param resultBitmap
	 *            the resulting bitmap defining which records are selected using
	 *            filtering, grouping, validation and combined time slices
	 * @param facts
	 *            the facts associated to the time-slice
	 */
	public DimExpressionEvaluator(final TidaIndex index, final String groupId,
			final long[] bounds, final List<TimeMemberRange> ranges,
			final Bitmap filterBitmap, final Bitmap resultBitmap,
			final FactDescriptorModelSet facts) {
		super(index, groupId);

		this.facts = facts;
		this.bounds = bounds;
		this.ranges = ranges;
		this.filterBitmap = filterBitmap;
		this.resultBitmap = resultBitmap;
	}

	@Override
	protected double applyFunction(final IAggregationFunction func) {
		return applyFunction(func, null);
	}

	@Override
	protected double applyFunction(final IAggregationFunction func,
			final IFactsHolder facts) {

		if (func instanceof IDimAggregationFunction) {

			/*
			 * If we have observers, we have to inform them, this is not that
			 * easy, because the data we have is already aggregated across the
			 * members.
			 */
			if (hasObserver()) {
				for (final TimeMemberRange range : this.ranges) {
					final long start = Math.max(bounds[0], range.getStart());
					final long end = Math.min(bounds[1], range.getEnd());

					// get the slices for the range
					final SliceWithDescriptors<?>[] slices = getIndex()
							.getIntervalIndexSlices(start, end);

					// combine the slices and facts
					long timepoint = start;
					for (final SliceWithDescriptors<?> slice : slices) {
						if (slice == null || slice.getBitmap() == null) {
							if (!notifyObservers(timepoint, null)) {
								return getCancellationFlag();
							}

							continue;
						} else {

							// create the combination
							final Bitmap bitmap = Bitmap.combineTimeAndFilter(
									getIndex().getIndexFactory(),
									slice.getBitmap(), this.filterBitmap);

							// cancel if needed
							if (!notifyObservers(timepoint, bitmap)) {
								return getCancellationFlag();
							}

							timepoint++;
						}
					}
				}
			}

			// check the result to be returned
			if (facts == null) {
				return func.getDefaultValue();
			} else {
				return ((IDimAggregationFunction) func).aggregate(getIndex(),
						resultBitmap, facts);
			}
		} else {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1022, IDimAggregationFunction.class.getSimpleName(),
					func == null ? null : func.getClass().getSimpleName(), func);
		}
	}

	@Override
	protected IFactsHolder getFactsHolder(final String modelId) {
		return new MapFactsDescriptorBased(facts.getDescriptors(modelId),
				getIndex(), resultBitmap);
	}

	@Override
	protected boolean useFunctionDirectly() {
		return facts == null || resultBitmap == null;
	}
}
