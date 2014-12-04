package net.meisen.dissertation.model.measures;

import net.meisen.dissertation.exceptions.GeneralException;
import net.meisen.dissertation.impl.measures.MapFactsDescriptorBased;
import net.meisen.dissertation.model.descriptors.FactDescriptor;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.dissertation.model.indexes.datarecord.slices.Slice;
import net.meisen.dissertation.model.util.IDoubleIterator;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Objects;

/**
 * Base implementation of an {@code AggregationFunction}.
 * 
 * @author pmeisen
 * 
 */
public abstract class BaseAggregationFunction implements IAggregationFunction {
	private Class<? extends IAggregationFunction> definedType = null;

	@Override
	public void setDefinedType(final Class<? extends IAggregationFunction> type)
			throws ForwardedRuntimeException {
		if (type == null || !type.isAssignableFrom(this.getClass())) {
			throw new ForwardedRuntimeException(GeneralException.class, 1001,
					getName(), type.getSimpleName());
		}

		this.definedType = type;
	}

	@Override
	public Class<? extends IAggregationFunction> getDefinedType() {
		return definedType;
	}

	@Override
	public String toString() {
		return getName().toUpperCase();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == this) {
			return true;
		} else if (obj == null) {
			return false;
		} else if (obj.getClass().equals(this.getClass())) {
			return Objects.equals(getDefinedType(),
					((IAggregationFunction) obj).getDefinedType());
		} else {
			return false;
		}
	}

	/**
	 * Helper method to calculate the sum of the specified {@code facts}.
	 * 
	 * @param facts
	 *            the facts to calculate the sum for
	 * 
	 * @return the sum of the facts
	 */
	public double sum(final IFactsHolder facts) {

		// make sure we have values
		if (facts == null || facts.amountOfFacts() == 0) {
			return getDefaultValue();
		} else {

			// otherwise get the sum and calculate the average
			double sum = 0.0;
			final IDoubleIterator it = facts.factsIterator();
			while (it.hasNext()) {
				sum += it.next();
			}

			return sum;
		}
	}

	/**
	 * Helper method to calculate the sum of the specified {@code results}.
	 * 
	 * @param results
	 *            the results to calculate the sum for
	 * 
	 * @return the sum of the results
	 */
	public double sum(final IResultsHolder results) {

		if (results == null || results.amountOfResults() == 0) {
			return getDefaultValue();
		} else {
			final IDoubleIterator it = results.resultsIterator();

			double res = 0.0;
			while (it.hasNext()) {
				res += it.next();
			}

			return res;
		}
	}

	/**
	 * Helper method to calculate the sum of the specified {@code facts}.
	 * 
	 * @param index
	 *            the {@code TidaIndex} to retrieve the meta-slices from
	 * @param bitmap
	 *            the defined bitmap of the filter and group
	 * @param descriptors
	 *            the descriptors of the fact
	 * 
	 * @return the sum of the facts
	 */
	public double sum(final TidaIndex index, final Bitmap bitmap,
			final FactDescriptorSet descriptors) {
		if (bitmap == null || descriptors == null) {
			return getDefaultValue();
		} else if (bitmap.determineCardinality() == 0) {
			return getDefaultValue();
		}

		if (descriptors.containsVariantRecords()) {

			// use the implementation of the factHolders to handle this
			return sum(new MapFactsDescriptorBased(descriptors, index, bitmap));
		} else {
			double sum = 0.0;
			for (final FactDescriptor<?> desc : descriptors) {

				// get the slice
				final Slice<?> metaSlice = index.getMetaIndexDimensionSlice(
						desc.getModelId(), desc.getId());

				// get the bitmap
				final Bitmap bmp = bitmap.and(metaSlice.getBitmap());
				sum += bmp.determineCardinality() * desc.getFact();
			}

			return sum;
		}
	}

	@Override
	public IAggregationFunction create() throws ForwardedRuntimeException {
		try {
			return this.getClass().newInstance();
		} catch (final Exception e) {
			throw new ForwardedRuntimeException(GeneralException.class, 1000,
					getName());
		}
	}

	@Override
	public double getDefaultValue() {
		return Double.NaN;
	}
}
