package net.meisen.dissertation.model.indexes.datarecord.slices;

import java.util.ArrayList;
import java.util.List;

import com.googlecode.javaewah.EWAHCompressedBitmap;

/**
 * A {@code IndexDimensionSlice} which is created by combining other slices.
 * 
 * @author pmeisen
 * 
 */
public class CombinedIndexDimensionSlice implements IIndexDimensionSlice {
	private EWAHCompressedBitmap bitmap;

	/**
	 * Types of aggregation/combination
	 */
	protected enum LogicType {
		/**
		 * Combine slices with {@code or}.
		 */
		OR,
		/**
		 * Combine slices with {@code and}.
		 */
		AND;
	}

	/**
	 * The constructor to create a not further specified combination.
	 */
	public CombinedIndexDimensionSlice() {
		this.bitmap = null;
	}

	/**
	 * Combines the {@code this} with the specified {@code slices} using an
	 * {@code and}-logic.
	 * 
	 * @param slices
	 *            the slices to be combined with {@code this}
	 */
	public void and(final Object... slices) {
		if (slices.length == 0) {
			return;
		} else {
			final EWAHCompressedBitmap[] mod = transform(slices, LogicType.AND);
			if (mod != null) {
				bitmap = EWAHCompressedBitmap.and(mod);
			}
		}
	}

	/**
	 * The cardinality of the {@code and}-combination, this method does not
	 * change the underlying bitmap.
	 * 
	 * @param slices
	 *            the slices to be combined with {@code this}
	 * 
	 * @return the cardinality of the result
	 */
	public int andCardinality(final Object... slices) {
		if (slices.length == 0) {
			return 0;
		} else {
			final EWAHCompressedBitmap[] mod = transform(slices, LogicType.AND);
			return mod == null ? count() : EWAHCompressedBitmap
					.andCardinality(mod);
		}
	}

	/**
	 * Combines the {@code this} with the specified {@code slices} using an
	 * {@code or}-logic.
	 * 
	 * @param slices
	 *            the slices to be combined with {@code this}
	 */
	public void or(final Object... slices) {
		if (slices.length == 0) {
			return;
		} else {
			final EWAHCompressedBitmap[] mod = transform(slices, LogicType.OR);
			if (mod != null) {
				bitmap = EWAHCompressedBitmap.or(mod);
			}
		}
	}

	/**
	 * The cardinality of the {@code or}-combination, this method does not
	 * change the underlying bitmap.
	 * 
	 * @param slices
	 *            the slices to be combined with {@code this}
	 * 
	 * @return the cardinality of the result
	 */
	public int orCardinality(final Object... slices) {
		if (slices.length == 0) {
			return 0;
		} else {
			final EWAHCompressedBitmap[] mod = transform(slices, LogicType.OR);
			return mod == null ? count() : EWAHCompressedBitmap
					.orCardinality(mod);
		}
	}

	/**
	 * Transforms the {@code slices} to an array of {@code EWAHCompressedBitmap}
	 * , which can be used to determine the result of applying the {@code type}
	 * to the {@code LogicType}.
	 * 
	 * @param slices
	 *            the slices to be transformed
	 * @param type
	 *            the {@code LogicType} which will be applied to the slices
	 * 
	 * @return the transformed array
	 */
	protected EWAHCompressedBitmap[] transform(final Object[] slices,
			final LogicType type) {

		if (slices != null && slices.length > 0) {

			// determine if we need an offset or not
			final int offset = bitmap == null ? 0 : 1;
			final int maxSize = slices.length + offset;

			// create a new list of bitmaps
			final List<EWAHCompressedBitmap> list = new ArrayList<EWAHCompressedBitmap>(
					maxSize);

			// add the bitmaps of the slices
			for (int i = 0; i < slices.length; i++) {
				final Object o = slices[i];

				if (o instanceof IIndexDimensionSlice) {
					final EWAHCompressedBitmap map = ((IIndexDimensionSlice) o)
							.getBitmap();

					/*
					 * ignore null maps, because it's not clear if those means
					 * all zeros or all ones
					 */
					if (map != null) {
						list.add(map);
					}
				} else if (o == null) {
					/*
					 * Determine how null-slices (null-slices are handled as
					 * 0-bitmap) effect the result. If the logic is or, it's not
					 * necessary to add any null-slices, because they don't
					 * change the result at all. Otherwise if the logic is and,
					 * including null would mean that the result is a 0-bitmap.
					 */
					if (LogicType.AND.equals(type)) {
						return new EWAHCompressedBitmap[] { IIndexDimensionSlice.EMPTY_BITMAP };
					}
				}
			}

			// add this one if there is one
			if (bitmap != null) {
				list.add(bitmap);
			} else if (list.size() == 0) {
				return new EWAHCompressedBitmap[] { IIndexDimensionSlice.EMPTY_BITMAP };
			}

			// create an array from the list
			return list.toArray(new EWAHCompressedBitmap[] {});
		} else {
			return null;
		}
	}

	@Override
	public int[] get() {
		return bitmap.toArray();
	}

	@Override
	public int count() {
		return bitmap == null ? 0 : bitmap.cardinality();
	}

	@Override
	public EWAHCompressedBitmap getBitmap() {
		return bitmap == null ? IIndexDimensionSlice.EMPTY_BITMAP : bitmap;
	}

	@Override
	public String toString() {
		return getBitmap().toString();
	}

	@Override
	public void optimize() {
		if (bitmap != null) {
			bitmap.trim();
		}
	}
}
