package net.meisen.dissertation.model.cache;

import java.util.Collection;

import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;

/**
 * A cache used to hold a specific part of bitmaps in memory and all bitmaps at
 * a location specified by the concrete implementation (e.g. memory,
 * file-system).
 * 
 * @author pmeisen
 * 
 */
public interface IBitmapCache {

	/**
	 * Initializes the {@code BitmapCache} for the specified {@code model}.
	 * 
	 * @param model
	 *            the {@code TidaModel} to initialize the cache for
	 * 
	 * @see TidaModel
	 */
	public void initialize(final TidaModel model);

	/**
	 * Configures the concrete {@code Cache} implementation.
	 * 
	 * @param configuration
	 *            the configuration to be used for the concrete {@code Cache}
	 *            implementation
	 */
	public void setConfig(final IBitmapCacheConfig configuration);

	/**
	 * Caches the specified {@code bitmap} for the specified {@code bitmapId}.
	 * 
	 * @param bitmapId
	 *            the {@code BitmapId} of the {@code bitmap} to be cached
	 * @param bitmap
	 *            the {@code Bitmap} to be cached
	 * 
	 * @see BitmapId
	 * @see Bitmap
	 */
	public void cacheBitmap(final BitmapId<?> bitmapId, final Bitmap bitmap);

	/**
	 * Checks if the cache contains a bitmap with the specified {@code bitmapId}
	 * .
	 * 
	 * @param bitmapId
	 *            the identifier to be checked
	 * 
	 * @return {@code true} if an instance is contained, otherwise {@code false}
	 */
	public boolean contains(final BitmapId<?> bitmapId);

	/**
	 * Gets a collection of all the cached {@code BitmapId} instances.
	 * 
	 * @return a collection of all the cached {@code BitmapId} instances
	 */
	public Collection<BitmapId<?>> getBitmapIdentifiers();

	/**
	 * Releases all of the resources used by the cache.
	 */
	public void release();

	/**
	 * Method used to disable or enable (default) the persistency. During the
	 * time the persistency is disable all changes are just done within the
	 * memory. Whenever the persistency is enabled again, all data have to be
	 * persisted if supported by the cache.
	 * 
	 * @param enable
	 *            {@code true} to enable persistency, otherwise {@code false}
	 * 
	 * @return the old value of the persistency setting
	 */
	public boolean setPersistency(final boolean enable);
}
