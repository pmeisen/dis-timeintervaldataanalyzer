package net.meisen.dissertation.model.indexes.datarecord.slices;

import java.util.Arrays;

import com.googlecode.javaewah.EWAHCompressedBitmap;

/**
 * A slice of an index's dimension, i.e. from a data point of view the slice
 * represents a value for each record of a specific value of a dimension. Let's
 * assume we have the dimension family with different family members:
 * {@code Mom - Debbie}, {@code Dad - Philipp}, {@code Child - Edison}. A slice
 * represents one value of the dimension e.g. {@code Mom - Debbie}. The slice
 * has a value, i.e. {@code 1} or {@code 0} for each record added. Whenever a
 * record has the value {@code Mom - Debbie} the slice will have a value of
 * {@code 1} for the record's position, otherwise the value will be {@code 0}.
 * 
 * @author pmeisen
 * 
 * @param <I>
 *            the type of the identifier used to identify the slice
 */
public class IndexDimensionSlice<I> implements
		Comparable<IndexDimensionSlice<I>>, IIndexDimensionSlice {

	private final I id;
	private EWAHCompressedBitmap bitmap;

	/**
	 * Creates a slice with no records added (i.e. everything is set to
	 * {@code 0}) for the specified {@code sliceId}.
	 * 
	 * @param sliceId
	 *            the identifier the slice stands for
	 */
	public IndexDimensionSlice(final I sliceId) {
		this(sliceId, null);
	}

	/**
	 * Creates a slice with no the specified {@code recId} added (i.e.
	 * everything is set to {@code 0}) for the specified {@code sliceId}.
	 * 
	 * @param sliceId
	 *            the identifier the slice stands for
	 * @param recordIds
	 *            the identifiers of the records to be set
	 */
	public IndexDimensionSlice(final I sliceId, final int... recordIds) {
		this.id = sliceId;

		if (recordIds != null) {
			Arrays.sort(recordIds);
			this.bitmap = EWAHCompressedBitmap.bitmapOf(recordIds);
		} else {
			this.bitmap = EWAHCompressedBitmap.bitmapOf();
		}
	}

	/**
	 * Gets the id of the slice.
	 * 
	 * @return the id of the slice
	 */
	public I getId() {
		return id;
	}

	/**
	 * Marks the specified {@code recordIds} to be set, i.e. the value
	 * {@code this} slice represents by id is assumed to be set for the
	 * specified records.
	 * 
	 * @param recordIds
	 *            the identifiers of the records to be set
	 */
	public void set(final int... recordIds) {
		if (recordIds == null || recordIds.length == 0) {
			return;
		}

		// sort the data and create the bitmap
		Arrays.sort(recordIds);
		bitmap = bitmap.or(EWAHCompressedBitmap.bitmapOf(recordIds));
	}

	/**
	 * Marks the specified {@code recId} to be set, i.e. the value {@code this}
	 * slice represents by id is assumed to be set for the specified record.
	 * 
	 * @param recId
	 *            the identifiers of the record to be set
	 */
	public void set(final int recId) {
		bitmap = bitmap.or(EWAHCompressedBitmap.bitmapOf(recId));
	}

	@Override
	public int[] get() {
		return bitmap.toArray();
	}

	@Override
	public int count() {
		return bitmap.cardinality();
	}

	/**
	 * The implementation of comparable compares the amount of slices with each
	 * other.
	 */
	@Override
	public int compareTo(final IndexDimensionSlice<I> slice) {
		if (slice == null) {
			return -1;
		} else {
			final boolean larger = get().length >= slice.get().length;
			return larger ? 1 : -1;
		}
	}

	@Override
	public EWAHCompressedBitmap getBitmap() {
		return bitmap;
	}

	@Override
	public String toString() {
		return id.toString() + " (" + bitmap + ")";
	}

	@Override
	public void optimize() {
		bitmap.trim();
	}
}
