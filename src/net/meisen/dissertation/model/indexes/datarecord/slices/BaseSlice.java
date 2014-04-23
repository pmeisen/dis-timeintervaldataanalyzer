package net.meisen.dissertation.model.indexes.datarecord.slices;

import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.IBitmapContainer;

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
public abstract class BaseSlice<I extends Object> implements
		Comparable<BaseSlice<I>>, IBitmapContainer {
	private final I id;

	/**
	 * The {@code Bitmap} of the slice
	 */
	protected final Bitmap bitmap;

	/**
	 * Creates a slice with no the specified {@code recId} added (i.e.
	 * everything is set to {@code 0}) for the specified {@code sliceId}.
	 * 
	 * @param sliceId
	 *            the identifier the slice stands for
	 * @param factory
	 *            factory used to create bitmap indexes
	 * @param recordIds
	 *            the identifiers of the records to be set
	 */
	public BaseSlice(final I sliceId, final BaseIndexFactory factory,
			final int... recordIds) {
		this.id = sliceId;
		this.bitmap = factory.createBitmap();

		if (recordIds != null) {
			this.bitmap.set(recordIds);
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

	@Override
	public int compareTo(final BaseSlice<I> slice) {
		if (slice == null) {
			return -1;
		} else {
			final int[] ids = get();
			final int[] sliceIds = slice.get();

			if (ids.length > sliceIds.length) {
				return 1;
			} else if (ids.length < sliceIds.length) {
				return -1;
			} else {
				for (int i = 0; i < ids.length; i++) {
					if (ids[i] > sliceIds[i]) {
						return 1;
					} else if (ids[i] < sliceIds[i]) {
						return -1;
					} else {
						continue;
					}
				}

				return 0;
			}
		}
	}

	@Override
	public String toString() {
		return id.toString() + " (" + bitmap + ")";
	}

	@Override
	public Bitmap getBitmap() {
		return bitmap;
	}

	/**
	 * Optimize the slice considering space and/or performance.
	 */
	public void optimize() {
		bitmap.optimize();
	}

	/**
	 * Get the identifiers of the records set for the slice.
	 * 
	 * @return the records' identifiers associated to the slice
	 */
	public int[] get() {
		return bitmap.getIds();
	}

	/**
	 * Counts the number of records associated to the slice.
	 * 
	 * @return the number of records associated to the slice
	 */
	public int count() {
		return bitmap.determineCardinality();
	}
}
