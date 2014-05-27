package net.meisen.dissertation.model.cache;

import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;

/**
 * Marks an instance to be the owner of a instance associated to a
 * {@code BitmapId}. There can only be one owner and the owner has the
 * responsibility to release a instance if asked for. Releasing means that the
 * pointer to the instance is removed so that garbage collection can take place.
 * 
 * @author pmeisen
 * 
 */
public interface IBitmapIdOwner {

	/**
	 * Gets the identifier of the {@code Bitmap} {@code this} owns.
	 * 
	 * @return the identifier of the {@code Bitmap} {@code this} owns
	 */
	public BitmapId<?> getBitmapId();

	/**
	 * Informs the {@code BitmapOwner} to release the cacheable. Releasing means
	 * that all pointers to the {@code Bitmap} are set to {@code null} so that
	 * garbage collection can take place.
	 * 
	 * @param instance
	 *            the instance to be released
	 */
	public void release(final IBitmapIdCacheable instance);
}
