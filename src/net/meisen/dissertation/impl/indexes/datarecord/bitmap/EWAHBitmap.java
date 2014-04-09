package net.meisen.dissertation.impl.indexes.datarecord.bitmap;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;

import com.googlecode.javaewah.EWAHCompressedBitmap;

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
	private EWAHCompressedBitmap bitmap;

	/**
	 * Default constructor used to create an empty bitmap, i.e. everything is
	 * set to {@code false}.
	 */
	public EWAHBitmap() {
		this.bitmap = EWAHCompressedBitmap.bitmapOf();
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
	public void set(final int... recordIds) {
		// sort the data and create the bitmap
		Arrays.sort(recordIds);
		bitmap = bitmap.or(EWAHCompressedBitmap.bitmapOf(recordIds));
	}

	@Override
	public void serialize(final DataOutputStream out) throws IOException {
		bitmap.serialize(out);
	}

	@Override
	public void deserialize(final DataInputStream in) throws IOException {
		bitmap.deserialize(in);
	}

	@Override
	public EWAHBitmap invert(final int pos) {

		// check if we even have a position
		if (pos < 0) {
			return new EWAHBitmap();
		}

		// let's get the current positions
		final int[] current = bitmap.toArray();
		final int currentSize = current.length;

		// determine the amount of set positions
		final int posPos = Arrays.binarySearch(current, pos) + 1;

		// calculate the size of the new array
		final int invertSize = pos + 1 + (currentSize - posPos)
				+ (posPos < 1 ? 1 : -1) * posPos;

		// create the result and a meaningful synonym in the context
		final int[] invert = new int[invertSize];
		final int lastPos = currentSize;
		final int finalPos = Math.max(currentSize, pos + 1);

		// iterate over all the positions and invert those
		int fromPos = 0, invertPos = 0;
		for (int i = 0; i < finalPos; i++) {
			final int foundPos = Arrays.binarySearch(current, fromPos, lastPos,
					i);

			if (foundPos > -1) {
				fromPos = foundPos + 1;

				// add it if we are at a position not to be inverted
				if (i > pos) {
					invert[invertPos] = i;
					invertPos++;
				}
			} else {
				fromPos = -1 * (foundPos + 1);

				// add the inverted value
				invert[invertPos] = i;
				invertPos++;
			}
		}

		return new EWAHBitmap(EWAHCompressedBitmap.bitmapOf(invert));
	}

	@Override
	public int invertCardinality(final int position) {
		return invert(position).determineCardinality();
	}

	@Override
	public EWAHBitmap and(final Bitmap... bitmaps) {
		return new EWAHBitmap(EWAHCompressedBitmap.and(createArray(true,
				bitmaps)));
	}

	@Override
	public int andCardinality(final Bitmap... bitmaps) {
		return EWAHCompressedBitmap.andCardinality(createArray(true, bitmaps));
	}

	@Override
	public EWAHBitmap or(final Bitmap... bitmaps) {
		return new EWAHBitmap(
				EWAHCompressedBitmap.or(createArray(true, bitmaps)));
	}

	@Override
	public int orCardinality(final Bitmap... bitmaps) {
		return EWAHCompressedBitmap.orCardinality(createArray(true, bitmaps));
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
}
