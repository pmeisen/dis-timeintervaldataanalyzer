package net.meisen.dissertation.model.indexes.datarecord.slices;

import java.io.DataInputStream;
import java.io.IOException;
import java.lang.ref.WeakReference;

import net.meisen.dissertation.exceptions.TidaIndexException;
import net.meisen.dissertation.model.cache.IBitmapIdCache;
import net.meisen.dissertation.model.cache.IBitmapIdCacheable;
import net.meisen.dissertation.model.cache.IBitmapIdOwner;
import net.meisen.dissertation.model.cache.IReferenceMechanismCache;
import net.meisen.dissertation.model.cache.IReleaseMechanismCache;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

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
		Comparable<BaseSlice<I>>, IBitmapContainer, IBitmapIdOwner {
	private final SliceId<I> id;
	private final IBitmapIdCache<Bitmap> cache;

	private Bitmap bitmap = null;
	private WeakReference<Bitmap> refBitmap = null;

	/**
	 * Creates a slice with no the specified {@code recId} added (i.e.
	 * everything is set to {@code 0}) for the specified {@code sliceId}.
	 * 
	 * @param sliceId
	 *            the identifier the slice stands for
	 * @param cache
	 *            the cache used for the {@code Bitmap} instances
	 */
	public BaseSlice(final SliceId<I> sliceId,
			final IBitmapIdCache<Bitmap> cache) {
		this.id = sliceId;
		this.cache = cache;

		// register the instance as user
		if (cache instanceof IReleaseMechanismCache) {
			((IReleaseMechanismCache<?, ?>) cache).registerOwner(this);
		}
	}

	/**
	 * Gets the id of the slice.
	 * 
	 * @return the id of the slice
	 */
	public I getId() {
		return id.getId();
	}

	/**
	 * Gets the identifier of the slice.
	 * 
	 * @return the identifier of the slice
	 */
	public SliceId<I> getSliceId() {
		return id;
	}

	/**
	 * Deserializes the {@code Bitmap} of the {@code BaseSlice} from the
	 * specified {@code in}.
	 * 
	 * @param in
	 *            the {@code InputStream} to read the information from
	 * 
	 * @throws IOException
	 *             if the data cannot be read
	 */
	protected void deserializeBitmap(final DataInputStream in)
			throws IOException {

		// load the bitmap from the serialized version
		getBitmap().deserialize(in);

		// inform the cache
		updateBitmapCache();
	}

	/**
	 * Updates the cache by caching the current bitmap of the slice.
	 */
	protected void updateBitmapCache() {
		cache.cache(getSliceId(), getBitmap());
	}

	@Override
	public BitmapId<?> getBitmapId() {
		return getSliceId();
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
		return id.toString() + " (" + getBitmap() + ")";
	}

	@SuppressWarnings("unchecked")
	@Override
	public Bitmap getBitmap() {
		Bitmap result;

		if (bitmap != null) {
			result = bitmap;
		} else if (refBitmap != null && (result = refBitmap.get()) != null) {
			// do nothing the result is set
		} else {
			if (cache instanceof IReleaseMechanismCache) {
				bitmap = ((IReleaseMechanismCache<BitmapId<I>, Bitmap>) cache)
						.get(getSliceId());
				refBitmap = null;

				result = bitmap;
			} else if (cache instanceof IReferenceMechanismCache) {
				bitmap = null;
				result = ((IReferenceMechanismCache<BitmapId<I>, Bitmap>) cache)
						.get(getSliceId());

				// just keep a weakReference
				refBitmap = new WeakReference<Bitmap>(result);
			} else {
				throw new ForwardedRuntimeException(TidaIndexException.class,
						1002,
						IReleaseMechanismCache.class.getSimpleName()
								+ ", "
								+ IReferenceMechanismCache.class
										.getSimpleName());
			}
		}

		return result;
	}

	@Override
	public void release(final IBitmapIdCacheable instance) {
		if (instance instanceof Bitmap) {
			bitmap = null;
			refBitmap = null;
		}
	}

	/**
	 * Optimize the slice considering space and/or performance.
	 */
	public void optimize() {
		getBitmap().optimize();
	}

	/**
	 * Get the identifiers of the records set for the slice.
	 * 
	 * @return the records' identifiers associated to the slice
	 */
	public int[] get() {
		return getBitmap().getIds();
	}

	/**
	 * Counts the number of records associated to the slice.
	 * 
	 * @return the number of records associated to the slice
	 */
	public int count() {
		return getBitmap().determineCardinality();
	}

	/**
	 * Checks if a strong-reference is used.
	 * 
	 * @return {@code true} if a strong-reference is used for the bitmap,
	 *         otherwise {@code false}
	 */
	public boolean isStrongReference() {
		getBitmap();

		return refBitmap == null;
	}

	/**
	 * Checks if a weak-reference is used.
	 * 
	 * @return {@code true} if a weak-reference is used for the bitmap,
	 *         otherwise {@code false}
	 */
	public boolean isWeakReference() {
		getBitmap();

		return refBitmap != null;
	}
}
