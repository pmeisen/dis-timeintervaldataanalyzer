package net.meisen.dissertation.model.indexes.datarecord.slices;

import java.io.DataInputStream;
import java.io.IOException;

import net.meisen.dissertation.model.cache.IBitmapIdCache;

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
public class Slice<I extends Object> extends BaseSlice<I> {

	/**
	 * Creates a slice with no the specified {@code recId} added (i.e.
	 * everything is set to {@code 0}) for the specified {@code sliceId}.
	 * 
	 * @param sliceId
	 *            the identifier the slice stands for
	 * @param cache
	 *            the {@code BitmapCache} to be used
	 */
	public Slice(final SliceId<I> sliceId, final IBitmapIdCache<Bitmap> cache) {
		super(sliceId, cache);
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

		// modify the bitmap
		getBitmap().set(recordIds);

		// inform the cache
		updateBitmapCache();
	}

	/**
	 * Deserializes the bitmap retrieved from the specified {@code in}.
	 * 
	 * @param in
	 *            the {@code InputStream} to read the information from
	 * 
	 * @throws IOException
	 *             if the data cannot be read
	 */
	public void deserialize(final DataInputStream in) throws IOException {
		super.deserializeBitmap(in);
	}
}
