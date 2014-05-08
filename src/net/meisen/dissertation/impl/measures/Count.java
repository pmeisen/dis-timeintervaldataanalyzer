package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.dissertation.model.measures.BaseAggregationFunction;
import net.meisen.dissertation.model.measures.IFactsHolder;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Aggregations method to count the amount of intervals.
 * 
 * @author pmeisen
 * 
 */
public class Count extends BaseAggregationFunction {
	private final static Logger LOG = LoggerFactory.getLogger(Count.class);

	private final static String name = "count";

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