package net.meisen.dissertation.impl.indexes.datarecord.slices;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.util.IIntIterator;

import com.googlecode.javaewah.EWAHCompressedBitmap;
import com.googlecode.javaewah.IntIterator;

/**
 * Implementation of a {@code Bitmap} using the {@code EWAHCompressedBitmap}
 * implementation.
 * 
 * @see EWAHCompressedBitmap
 * 
 * @author pmeisen
 * 
 */
public class EWAHBitmap extends Bitmap {
	private final static int MAX_SIZE = Integer.MAX_VALUE
			- EWAHCompressedBitmap.wordinbits;
	private EWAHCompressedBitmap bitmap;
	private int cardinality = -1;

	/**
	 * Default constructor used to create an empty bitmap, i.e. everything is
	 * set to {@code false}.
	 */
	public EWAHBitmap() {
		this(EWAHCompressedBitmap.bitmapOf());
	}

	/**
	 * Internally used constructor to create a new instance of a bitmap based on
	 * a {@code EWAHCompressedBitmap}.
	 * 
	 * @param bitmap
	 *            the {@code EWAHCompressedBitmap} the newly created instance is
	 *            based on
	 */
	protected EWAHBitmap(final EWAHCompressedBitmap bitmap) {
		this.bitmap = bitmap;
	}

	@Override
	public int[] getIds() {
		return bitmap.toArray();
	}

	@Override
	public int determineCardinality() {
		return bitmap.cardinality();
	}

	@Override
	public void optimize() {
		// bitmap.trim();
	}

	@Override
	protected void set(final int... recordIds) {
		// sort the data and create the bitmap
		Arrays.sort(recordIds);
		bitmap = bitmap.or(EWAHCompressedBitmap.bitmapOf(recordIds));
	}

	@Override
	public void serialize(final DataOutput out) throws IOException {
		bitmap.serialize(out);
	}

	@Override
	public void deserialize(final DataInput in) throws IOException {
		bitmap.deserialize(in);
	}

	@Override
	public EWAHBitmap invert(final int position) {
		return new EWAHBitmap(EWAHCompressedBitmap.xor(
				createAllSetBitmap(position), bitmap));
	}

	@Override
	public int invertCardinality(final int position) {
		if (cardinality == -1) {
			cardinality = invert(position).determineCardinality();
		}

		return cardinality;
	}

	/**
	 * Create a bitmap with all values set up to the specified {@code position}.
	 * 
	 * @param position
	 *            the position (zero-based) to define the set values
	 * 
	 * @return the created bitmap
	 */
	protected EWAHCompressedBitmap createAllSetBitmap(final int position) {

		final EWAHCompressedBitmap bitmap;
		if (position < getMaxId()) {
			bitmap = EWAHCompressedBitmap.bitmapOf(position + 1);
			bitmap.not();
		} else {
			bitmap = EWAHCompressedBitmap.bitmapOf(position);
			bitmap.not();
			bitmap.set(position);
		}

		return bitmap;
	}

	@Override
	public EWAHBitmap and(final Bitmap... bitmaps) {
		final EWAHCompressedBitmap[] array = createArray(true, bitmaps);
		if (array.length > 1) {
			return new EWAHBitmap(EWAHCompressedBitmap.and(array));
		} else if (array.length > 0) {
			return copy();
		} else {
			return new EWAHBitmap();
		}
	}

	@Override
	public int andCardinality(final Bitmap... bitmaps) {
		final EWAHCompressedBitmap[] array = createArray(true, bitmaps);
		if (array.length > 0) {
			return EWAHCompressedBitmap.andCardinality(array);
		} else {
			return 0;
		}
	}

	@Override
	public EWAHBitmap xor(final Bitmap... bitmaps) {
		final EWAHCompressedBitmap[] array = createArray(true, bitmaps);
		if (array.length > 1) {
			return new EWAHBitmap(EWAHCompressedBitmap.xor(array));
		} else if (array.length > 0) {
			return copy();
		} else {
			return new EWAHBitmap();
		}
	}

	@Override
	public EWAHBitmap or(final Bitmap... bitmaps) {
		final EWAHCompressedBitmap[] array = createArray(true, bitmaps);
		if (array.length > 1) {
			return new EWAHBitmap(EWAHCompressedBitmap.or(array));
		} else if (array.length > 0) {
			return copy();
		} else {
			return new EWAHBitmap();
		}
	}

	@Override
	public int orCardinality(final Bitmap... bitmaps) {
		final EWAHCompressedBitmap[] array = createArray(true, bitmaps);
		if (array.length > 0) {
			return EWAHCompressedBitmap.orCardinality(array);
		} else {
			return 0;
		}
	}

	/**
	 * Creates an array of {@code EWAHCompressedBitmap} instances and optionally
	 * adds {@code this}.
	 * 
	 * @param addThis
	 *            {@code true} if the {@code EWAHCompressedBitmap} of
	 *            {@code this} should be added to the result
	 * @param bitmaps
	 *            the bitmaps to be transformed into an array of
	 *            {@code EWAHCompressedBitmap} instances
	 * 
	 * @return the returned array
	 */
	protected EWAHCompressedBitmap[] createArray(final boolean addThis,
			final Bitmap... bitmaps) {
		final int maxSize = (addThis ? 1 : 0) + bitmaps.length;
		final List<EWAHCompressedBitmap> list = new ArrayList<EWAHCompressedBitmap>(
				maxSize);

		// add this if asked for
		if (addThis) {
			list.add(this.bitmap);
		}

		// add all the EWAHCompressedBitmap
		for (final Bitmap bitmap : bitmaps) {

			// null values are ignored
			if (bitmap instanceof EWAHBitmap) {
				list.add(((EWAHBitmap) bitmap).bitmap);
			}
		}

		return list.toArray(new EWAHCompressedBitmap[list.size()]);
	}

	@Override
	public boolean equals(final Object o) {
		if (o == null) {
			return false;
		} else if (o == this) {
			return true;
		} else if (o instanceof EWAHBitmap) {
			return this.bitmap.equals(((EWAHBitmap) o).bitmap);
		} else {
			return false;
		}
	}

	@Override
	public String toString() {
		return bitmap.toString();
	}

	@Override
	public EWAHBitmap clone() {
		return copy();
	}

	@Override
	public EWAHBitmap copy() {
		try {
			return new EWAHBitmap(bitmap.clone());
		} catch (final CloneNotSupportedException e) {
			// ignore
			return null;
		}
	}

	@Override
	public int getMaxId() {
		return MAX_SIZE;
	}

	@Override
	public int getMinId() {
		return 0;
	}

	@Override
	public IIntIterator intIterator() {

		return new IIntIterator() {
			private final IntIterator it = bitmap.intIterator();

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
		return bitmap.intIterator().hasNext();
	}
}
