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

	public EWAHBitmap() {
		this.bitmap = EWAHCompressedBitmap.bitmapOf();
	}

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
	public Bitmap and(final Bitmap... bitmaps) {
		return new EWAHBitmap(EWAHCompressedBitmap.and(createArray(true,
				bitmaps)));
	}

	@Override
	public int andCardinality(final Bitmap... bitmaps) {
		return EWAHCompressedBitmap.andCardinality(createArray(true, bitmaps));
	}

	@Override
	public Bitmap or(final Bitmap... bitmaps) {
		return new EWAHBitmap(
				EWAHCompressedBitmap.or(createArray(true, bitmaps)));
	}

	@Override
	public int orCardinality(final Bitmap... bitmaps) {
		return EWAHCompressedBitmap.orCardinality(createArray(true, bitmaps));
	}

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
