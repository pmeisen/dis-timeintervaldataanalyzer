package net.meisen.dissertation.model.measures;

import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.Slice;

public abstract class BaseAggregationFunction implements IAggregationFunction {

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final Iterable<Descriptor<?, ?, ?>> descriptors) {
		throw new UnsupportedOperationException("Currently not supported!");
	}

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final double[] facts) {
		throw new UnsupportedOperationException("Currently not supported!");
	}

	@Override
	public String toString() {
		return getName().toUpperCase();
	}

	public double sum(final double[] facts) {

		// make sure we have values
		if (facts == null) {
			return 0.0;
		}

		// otherwise get the sum and calculate the average
		double sum = 0.0;
		for (final double fact : facts) {
			sum += fact;
		}

		return sum;
	}

	public double sum(final TidaIndex index, final Bitmap bitmap,
			final Iterable<Descriptor<?, ?, ?>> descriptors) {

		double sum = 0;
		for (final Descriptor<?, ?, ?> desc : descriptors) {

			if (desc.isRecordInvariant()) {

				// get the slice
				final Slice<?> metaSlice = index.getMetaIndexDimensionSlice(
						desc.getModelId(), desc.getId());

				// get the bitmap
				final Bitmap bmp = bitmap.and(metaSlice.getBitmap());
				sum += bmp.determineCardinality() * desc.getFactValue(null);

			} else {
				// TODO add support
				throw new UnsupportedOperationException(
						"Currently not supported!");
			}
		}

		return sum;
	}
}
