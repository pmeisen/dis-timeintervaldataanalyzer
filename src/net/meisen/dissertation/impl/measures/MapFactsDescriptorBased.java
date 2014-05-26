package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.descriptors.FactDescriptor;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.dissertation.model.indexes.datarecord.slices.Slice;

public class MapFactsDescriptorBased extends MapFactsArrayBased {
	private final FactDescriptorSet descriptors;

	private TidaIndex index;
	private Bitmap bitmap;

	public MapFactsDescriptorBased(final FactDescriptorSet descriptors,
			final TidaIndex index, final Bitmap bitmap) {
		super(index.getLastRecordId());

		this.index = index;
		this.bitmap = bitmap;
		this.descriptors = descriptors;
	}

	@Override
	public double getFactOfRecord(final int recordId) {
		fillMap(false);
		return super.getFactOfRecord(recordId);
	}

	protected double[] fillMap(final boolean returnSortedFacts) {
		if (size() > 0) {
			if (returnSortedFacts) {
				return super.sortedFacts();
			} else {
				return null;
			}
		}

		// add all the values of the descriptors
		final double[] facts = returnSortedFacts ? new double[index
				.getAmountOfRecords()] : null;
		for (final FactDescriptor<?> desc : descriptors) {

			if (desc.isRecordInvariant()) {
				final Slice<?> metaSlice = index.getMetaIndexDimensionSlice(
						desc.getModelId(), desc.getId());

				// get the bitmap
				final Bitmap bmp = bitmap == null ? metaSlice.getBitmap()
						: bitmap.and(metaSlice.getBitmap());
				final double val = desc.getFact();

				// add the values to the identifiers
				final int[] ids = bmp.getIds();
				for (int i = 0; i < ids.length; i++) {
					final int id = ids[i];

					// add the value and set it
					set(id, val);
					if (returnSortedFacts) {
						facts[i] = val;
					}
				}

			} else {
				// TODO add support
				throw new UnsupportedOperationException(
						"Currently not supported!");
			}
		}

		return facts;
	}

	@Override
	public int[] recordIds() {
		fillMap(false);
		return super.recordIds();
	}

	@Override
	public double[] facts() {
		fillMap(false);
		return super.facts();
	}

	@Override
	public double[] sortedFacts() {
		return fillMap(true);
	}
}
