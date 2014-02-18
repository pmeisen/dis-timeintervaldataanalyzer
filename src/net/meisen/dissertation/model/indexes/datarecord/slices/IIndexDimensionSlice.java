package net.meisen.dissertation.model.indexes.datarecord.slices;

import com.googlecode.javaewah.EWAHCompressedBitmap;

/**
 * A slice of a dimension, i.e. one bitmap for a specific value (or a range of
 * values) of the dimension.
 * 
 * @author pmeisen
 * 
 */
public interface IIndexDimensionSlice {
	/**
	 * 
	 */
	public static final EWAHCompressedBitmap EMPTY_BITMAP = EWAHCompressedBitmap.bitmapOf();

	/**
	 * Counts how many values are set.
	 * 
	 * @return the amount of set values
	 */
	public int count();

	/**
	 * Gets an array of all the set records identifiers.
	 * 
	 * @return an array of all the set records
	 */
	public int[] get();

	/**
	 * Gets the underlying bitmap.
	 * 
	 * @return the underlying bitmap of the slice
	 */
	public EWAHCompressedBitmap getBitmap();

	/**
	 * Optimizes the slice considering e.g. storage.
	 */
	public void optimize();
}
