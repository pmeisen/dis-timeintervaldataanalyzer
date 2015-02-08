package net.meisen.dissertation.impl.indexes.datarecord.slices;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.util.IIntIterator;

import org.roaringbitmap.FastAggregation;
import org.roaringbitmap.IntIterator;

/**
 * A bitmap based on {@link org.roaringbitmap.RoaringBitmap RoaringBitmap}.
 * 
 * @author pmeisen
 * 
 */
public class RoaringBitmap extends Bitmap {
	private org.roaringbitmap.RoaringBitmap bitmap;

	/**
	 * Default constructor used to create an empty bitmap, i.e. everything is
	 * set to {@code false}.
	 */
	public RoaringBitmap() {
		this(new org.roaringbitmap.RoaringBitmap());
	}

	/**
	 * Internally used constructor to create a new instance of a bitmap based on
	 * a {@code RoaringBitmap}.
	 * 
	 * @param bitmap
	 *            the {@code RoaringBitmap} the newly created instance is based
	 *            on
	 */
	protected RoaringBitmap(final org.roaringbitmap.RoaringBitmap bitmap) {
		this.bitmap = bitmap;
	}

	@Override
	public int[] getIds() {
		return bitmap.toArray();
	}

	@Override
	public int getMaxId() {
		return Integer.MAX_VALUE;
	}

	@Override
	public int getMinId() {
		return 0;
	}

	@Override
	public int determineCardinality() {
		return bitmap.getCardinality();
	}

	@Override
	public void optimize() {
		bitmap.trim();
	}

	@Override
	protected void set(final int... recordIds) {
		final org.roaringbitmap.RoaringBitmap clone = bitmap.clone();
		for (final int recordId : recordIds) {
			clone.add(recordId);
		}

		// now set the bitmap in just one step
		this.bitmap = clone;
	}

	@Override
	public RoaringBitmap clone() {
		return copy();
	}

	@Override
	public RoaringBitmap copy() {
		return new RoaringBitmap(bitmap.clone());
	}

	@Override
	public void serialize(final DataOutput out) throws IOException {
		bitmap.serialize(out);
	}

	@Override
	protected void deserialize(final DataInput in) throws IOException {
		bitmap.deserialize(in);
	}

	@Override
	public Bitmap invert(final int position) {
		final org.roaringbitmap.RoaringBitmap bmp;
		if (position == Integer.MAX_VALUE) {
			bmp = org.roaringbitmap.RoaringBitmap.flip(bitmap, 0, position);
			bmp.add(Integer.MAX_VALUE);
		} else {
			bmp = org.roaringbitmap.RoaringBitmap.flip(bitmap, 0, position + 1);
		}

		return new RoaringBitmap(bmp);
	}

	@Override
	public int invertCardinality(final int position) {
		return invert(position).determineCardinality();
	}

	@Override
	public RoaringBitmap and(final Bitmap... bitmaps) {
		return new RoaringBitmap(
				FastAggregation.and(createArray(true, bitmaps)));
	}

	@Override
	public int andCardinality(final Bitmap... bitmaps) {
		return and(bitmaps).determineCardinality();
	}

	@Override
	public RoaringBitmap or(final Bitmap... bitmaps) {
		return new RoaringBitmap(FastAggregation.or(createArray(true, bitmaps)));
	}

	@Override
	public RoaringBitmap xor(final Bitmap... bitmaps) {
		return new RoaringBitmap(FastAggregation.horizontal_xor(createArray(
				true, bitmaps)));
	}

	@Override
	public int orCardinality(final Bitmap... bitmaps) {
		return or(bitmaps).determineCardinality();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == this) {
			return true;
		} else if (obj instanceof RoaringBitmap) {
			return bitmap.equals(((RoaringBitmap) obj).bitmap);
		} else {
			return false;
		}
	}

	@Override
	public String toString() {
		return bitmap.toString();
	}

	/**
	 * Creates an array of {@code RoaringBitmap} instances and optionally adds
	 * {@code this}.
	 * 
	 * @param addThis
	 *            {@code true} if the {@code RoaringBitmap} of {@code this}
	 *            should be added to the result
	 * @param bitmaps
	 *            the bitmaps to be transformed into an array of
	 *            {@code RoaringBitmap} instances
	 * 
	 * @return the returned array
	 * 
	 * @see org.roaringbitmap.RoaringBitmap RoaringBitmap
	 */
	protected org.roaringbitmap.RoaringBitmap[] createArray(
			final boolean addThis, final Bitmap... bitmaps) {
		final int maxSize = (addThis ? 1 : 0) + bitmaps.length;
		final List<org.roaringbitmap.RoaringBitmap> list = new ArrayList<org.roaringbitmap.RoaringBitmap>(
				maxSize);

		// add this if asked for
		if (addThis) {
			list.add(this.bitmap);
		}

		// add all the RoaringBitmap
		for (final Bitmap bitmap : bitmaps) {

			// null values are ignored
			if (bitmap instanceof RoaringBitmap) {
				list.add(((RoaringBitmap) bitmap).bitmap);
			}
		}

		return list.toArray(new org.roaringbitmap.RoaringBitmap[list.size()]);
	}

	@Override
	public IIntIterator intIterator() {

		return new IIntIterator() {
			private final IntIterator it = bitmap.getIntIterator();

			@Override
			public int next() {
				return it.next();
			}

			@Override
			public boolean hasNext() {
				return it.hasNext();
			}
		};
	}

	@Override
	public boolean isBitSet() {
		return !bitmap.isEmpty();
	}
}
