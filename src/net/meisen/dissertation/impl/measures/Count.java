package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.dissertation.model.measures.BaseAggregationFunction;
import net.meisen.dissertation.model.measures.IDimAggregationFunction;
import net.meisen.dissertation.model.measures.IFactsHolder;
import net.meisen.dissertation.model.measures.ILowAggregationFunction;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Aggregations method to count the amount of intervals.
 * 
 * @author pmeisen
 * 
 */
public class Count extends BaseAggregationFunction implements
		ILowAggregationFunction, IDimAggregationFunction {
	private final static Logger LOG = LoggerFactory.getLogger(Count.class);

	private final static String name = "count";

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final FactDescriptorSet descriptors, final int timepoint) {
		return aggregate(index, bitmap, descriptors);
	}

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final IFactsHolder facts, final int timepoint) {
		return aggregate(index, bitmap, facts);
	}

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final FactDescriptorSet descriptors) {
		if (bitmap == null || descriptors == null) {
			return getDefaultValue();
		}

		return bitmap.determineCardinality();
	}

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final IFactsHolder facts) {
		if (LOG.isWarnEnabled()) {
			LOG.warn("Using count aggregation with complex expression, to increase performance remove any complex expression within a count-aggregation.");
		}

		if (bitmap == null || facts == null || facts.amountOfFacts() == 0) {
			return getDefaultValue();
		}

		return bitmap.determineCardinality();
	}

	@Override
	public String getName() {
		return name;
	}

	@Override
	public double getDefaultValue() {
		return 0.0;
	}
}
