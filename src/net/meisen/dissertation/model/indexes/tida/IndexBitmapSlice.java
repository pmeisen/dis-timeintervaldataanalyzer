package net.meisen.dissertation.model.indexes.tida;

import java.util.Arrays;

import com.googlecode.javaewah.EWAHCompressedBitmap;

public class IndexBitmapSlice<I> {

	private final I id;
	private EWAHCompressedBitmap bitmap;

	public IndexBitmapSlice(final I sliceId) {
		this.id = sliceId;
		this.bitmap = null;
	}

	public I getId() {
		return id;
	}

	public void set(final int... recordIds) {
		if (recordIds == null || recordIds.length == 0) {
			return;
		}

		// sort the data and create the bitmap
		Arrays.sort(recordIds);
		or(EWAHCompressedBitmap.bitmapOf(recordIds));
	}

	public void set(final int recId) {
		or(EWAHCompressedBitmap.bitmapOf(recId));
	}

	protected void or(final EWAHCompressedBitmap recBitmap) {

		if (bitmap == null) {
			bitmap = recBitmap;
		} else {
			bitmap = bitmap.or(recBitmap);
		}

		// cleanUp
		bitmap.trim();
	}

	public int[] get() {
		return bitmap.toArray();
	}
}
