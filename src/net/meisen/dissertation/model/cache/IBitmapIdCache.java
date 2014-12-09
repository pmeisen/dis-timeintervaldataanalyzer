package net.meisen.dissertation.model.cache;

import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;

/**
 * Interface for a cache which stores {@code BitmapId} related instances.
 * 
 * @author pmeisen
 * 
 * @param <T>
 *            the type of the instance stored by the cache
 */
public interface IBitmapIdCache<T extends IBitmapIdCacheable> extends ICache, Iterable<BitmapId<?>> {

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
	 * Checks if the cache contains an instance with the specified
	 * {@code bitmapId} .
	 * 
	 * @param bitmapId
	 *            the identifier to be checked
	 * 
	 * @return {@code true} if an instance is contained, otherwise {@code false}
	 */
	public boolean contains(final BitmapId<?> bitmapId);
}
