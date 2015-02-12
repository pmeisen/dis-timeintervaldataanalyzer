package net.meisen.dissertation.impl.measures;

import java.util.Collections;
import java.util.Iterator;

import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.FactDescriptor;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.dissertation.model.indexes.datarecord.slices.Slice;
import net.meisen.dissertation.model.measures.IFactsHolder;
import net.meisen.dissertation.model.util.IDoubleIterator;
import net.meisen.dissertation.model.util.IIntIterator;

/**
 * A map containing facts which is based on descriptors.
 * 
 * @author pmeisen
 * 
 */
public class MapFactsDescriptorBased implements IFactsHolder {

	private final class DoubleIterator implements IDoubleIterator {
		private final Iterator<FactDescriptor<?>> it;

		private double curFact = Double.NaN;
		private int amountOfFacts = -1;
		private int nextPos = 0;

		public DoubleIterator(final FactDescriptorSet descriptors,
				final boolean ascOrder, final boolean containNan) {

			if (descriptors == null) {
				it = Collections.<FactDescriptor<?>> emptyList().iterator();
			} else if (ascOrder) {
				it = containNan ? descriptors.iterator() : descriptors
						.nonNanIterator();
			} else {
				it = containNan ? descriptors.descendingIterator()
						: descriptors.descendingNonNanIterator();
			}
		}

		@Override
		public boolean hasNext() {
			return checkNext();
		}

		@Override
		public double next() {
			if (checkNext()) {

				nextPos++;
				return curFact;
			} else {
				throw new IllegalStateException("No next value available.");
			}
		}

		private boolean checkNext() {

			// make sure we have the current amount
			if (nextPos + 1 > amountOfFacts) {
				amountOfFacts = -1;

				while (amountOfFacts < 1) {
					if (it.hasNext()) {
						final FactDescriptor<?> desc = it.next();
						final Bitmap bmp = getBitmap(desc);

						curFact = desc.getFact();
						amountOfFacts = bmp.determineCardinality();
						nextPos = 0;
					} else {
						return false;
					}
				}

				return true;
			} else {
				return true;
			}
		}
	};

	private final FactDescriptorSet descriptors;
	private final TidaIndex index;
	private final Bitmap bitmap;

	private final MapFactsArrayBased array;

	private Bitmap nonNaNBitmap = null;

	/**
	 * Constructor to create a {@code MapFactsDescriptorBased} for the specified
	 * {@code FactDescriptorSet}.
	 * 
	 * @param descriptors
	 *            the {@code FactDescriptorSet} the facts descriptor is based on
	 * @param index
	 *            the index
	 * @param bitmap
	 *            the {@code Bitmap} which defines the pre-selection, i.e. a
	 *            filter or valid records
	 */
	public MapFactsDescriptorBased(final FactDescriptorSet descriptors,
			final TidaIndex index, final Bitmap bitmap) {

		this.index = index;
		
		// might be null, normally that should be covered in advance 
		this.bitmap = bitmap == null ? index.getIndexFactory().createBitmap()
				: bitmap;
		this.descriptors = descriptors == null ? new FactDescriptorSet()
				: descriptors;

		if (this.descriptors == null) {
			array = null;
		} else if (this.descriptors.containsVariantRecords()) {
			array = new MapFactsArrayBased();

			// set all the invariant once
			final Iterator<FactDescriptor<?>> itInvariants = descriptors
					.variantIterator();
			while (itInvariants.hasNext()) {
				final FactDescriptor<?> factDesc = itInvariants.next();
				final Bitmap bmp = getBitmap(factDesc);

				array.setAll(bmp.intIterator(), factDesc.getFact());
			}

			// set the variant once
			final Iterator<FactDescriptor<?>> itVariants = descriptors
					.variantIterator();
			while (itVariants.hasNext()) {
				final FactDescriptor<?> factDesc = itVariants.next();
				final Bitmap bmp = getBitmap(factDesc);
				final Descriptor<?, ?, ?> desc = index.getDescriptor(factDesc);

				// add each variant one by one
				final IIntIterator it = bmp.intIterator();
				while (it.hasNext()) {
					final int recId = it.next();

					final IDataRecord rec = index.getRecord(recId);
					array.set(recId, desc.getFactValue(rec));
				}
			}
		} else {
			array = null;
		}
	}

	/**
	 * Gets the bitmap of the {@code desc} combined with the selected
	 * identifiers of {@code this}.
	 * 
	 * @param desc
	 *            the {@code FactDescriptor} to get the bitmap for
	 * 
	 * @return the bitmap
	 */
	protected Bitmap getBitmap(final FactDescriptor<?> desc) {

		if (desc == null) {

			/*
			 * That's not allowed and not expected.
			 */
			throw new NullPointerException(
					"The descriptor should never be null at this point.");
		} else if (desc.isValueInvariant()) {

			/*
			 * Just get all the values, because they all have the same value, we
			 * don't have to filter by descriptor.
			 */
			return bitmap;
		} else if (desc.isRecordInvariant()) {
			final Slice<?> slice = index.getMetaIndexDimensionSlice(
					desc.getModelId(), desc.getId());

			/*
			 * Determine the combined bitmap of the slice and the filtering
			 * bitmap.
			 */
			if (slice == null) {
				return bitmap;
			} else if (bitmap == null) {
				return slice.getBitmap();
			} else {
				return bitmap.and(slice.getBitmap());
			}
		} else {

			/*
			 * We have a variant record, that must have been handled before.
			 */
			throw new IllegalStateException(
					"Variant records should have been handled as array.");
		}
	}

	@Override
	public double getFactOfRecord(final int recordId) {
		if (descriptors == null) {
			return Double.NaN;
		} else if (array == null) {
			final Bitmap recordBitmap = Bitmap.createBitmap(
					index.getIndexFactory(), recordId);

			for (final FactDescriptor<?> factDesc : descriptors) {
				final Bitmap bmp = getBitmap(factDesc);
				if (recordBitmap.and(bmp).isBitSet()) {
					return factDesc.getFact();
				}
			}

			return Double.NaN;
		} else {
			return array.getFactOfRecord(recordId);
		}
	}

	@Override
	public IIntIterator recordIdsIterator() {
		if (array == null) {
			return bitmap.intIterator();
		} else {
			return array.recordIdsIterator();
		}
	}

	@Override
	public IDoubleIterator iterator(final boolean excludeNaN) {
		if (array == null) {
			return new DoubleIterator(descriptors, true, !excludeNaN);
		} else {
			return array.iterator(excludeNaN);
		}
	}

	@Override
	public IDoubleIterator descSortedIterator() {
		if (array == null) {
			return new DoubleIterator(descriptors, false, true);
		} else {
			return array.descSortedIterator();
		}
	}

	@Override
	public IDoubleIterator sortedIterator() {
		if (array == null) {
			return iterator(false);
		} else {
			return array.sortedIterator();
		}
	}

	@Override
	public int amount() {
		return bitmap.determineCardinality();
	}

	/**
	 * The methods determines which implementation is used. If at least one
	 * {@code FactDescriptor} does not have invariant facts, the
	 * {@code MapFactsArrayBased} implementation is used by {@code this} in the
	 * background.
	 * 
	 * @return {@code true} if the {@code MapFactsArrayBased} implementation is
	 *         used in the background, otherwise {@code false}
	 */
	public boolean usesArrayImplementation() {
		return array != null;
	}

	@Override
	public int amountOfNonNaN() {
		if (array == null) {
			return getNonNaNBitmap().determineCardinality();
		} else {
			return array.amountOfNonNaN();
		}
	}

	@Override
	public int amountOfNaN() {
		if (array == null) {
			return amount() - amountOfNonNaN();
		} else {
			return array.amountOfNaN();
		}
	}

	/**
	 * Creates a bitmap containing only the non-NaN values of {@code this}.
	 * 
	 * @return the created bitmap
	 */
	protected Bitmap getNonNaNBitmap() {
		if (nonNaNBitmap == null) {
			final Iterator<FactDescriptor<?>> it = descriptors.nanIterator();

			Bitmap nanBitmap = null;
			while (it.hasNext()) {
				final FactDescriptor<?> desc = it.next();

				if (desc.isValueInvariant()) {
					/*
					 * All values of the DescriptorModel represented by this are
					 * NaN values, so there aren't any nonNaN-values.
					 */
					nonNaNBitmap = index.getIndexFactory().createBitmap();
					return nonNaNBitmap;
				} else {
					final Slice<?> slice = index.getMetaIndexDimensionSlice(
							desc.getModelId(), desc.getId());

					if (slice != null && slice.getBitmap() != null) {
						if (nanBitmap == null) {
							nanBitmap = slice.getBitmap();
						} else {
							nanBitmap = slice.getBitmap().or(nanBitmap);
						}
					}
				}
			}

			// get the NaNBitmap
			if (nanBitmap == null) {
				nonNaNBitmap = bitmap;
			} else {
				nonNaNBitmap = bitmap.and(bitmap.xor(nanBitmap));
			}
		}

		return nonNaNBitmap;
	}
}
