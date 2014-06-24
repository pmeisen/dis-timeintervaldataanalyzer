package net.meisen.dissertation.model.cache;

import java.util.Collection;

import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;

/**
 * Interface for a cache which stores {@code BitmapId} related instances.
 * 
 * @author pmeisen
 * 
 * @param <T>
 *            the type of the instance stored by the cache
 */
public interface IBitmapIdCache<T extends IBitmapIdCacheable> {

	/**
	 * Initializes the {@code Cache} for the specified {@code model}.
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
	public void setConfig(final IBitmapIdCacheConfig configuration);

	/**
	 * Caches the specified instance for the specified {@code bitmapId}.
	 * 
	 * @param bitmapId
	 *            the {@code BitmapId} of the instance to be cached
	 * @param instance
	 *            the instance to be cached
	 * 
	 * @see BitmapId
	 */
	public void cache(final BitmapId<?> bitmapId, final T instance);

	/**
	 * Gets a collection of all the cached {@code BitmapId} instances.
	 * 
	 * @return a collection of all the cached {@code BitmapId} instances
	 */
	public Collection<BitmapId<?>> getBitmapIdentifiers();

	/**
	 * Checks if the cache contains an instance with the specified
	 * {@code bitmapId} .
	 * 
	 * @param bitmapId
	 *            the identifier to be checked
	 * 
	 * @return {@code true} if an instance is contained, otherwise {@code false}
	 */
	public boolean contains(final BitmapId<?> bitmapId);

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
