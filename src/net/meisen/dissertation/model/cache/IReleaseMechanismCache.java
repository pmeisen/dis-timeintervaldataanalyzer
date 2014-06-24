package net.meisen.dissertation.model.cache;

/**
 * The release-mechanism defines that the cache is capable of informing an owner
 * whenever the cached instance is released from memory. The cache supporting
 * this mechanism has to keep track of the owners of the different instances
 * (i.e. the once that have to be informed). Additionally, it most know whenever
 * a cache is released from memory and trigger the
 * {@link IBitmapIdOwner#release(IBitmapIdCacheable)} method.
 * 
 * @author pmeisen
 * 
 * @param <K>
 *            the identifier of the cached instance
 * @param <T>
 *            the instance to be cached
 */
public interface IReleaseMechanismCache<K, T> {

	/**
	 * Registers a {@code BitmapOwner} within the cache. The {@code BitmapOwner}
	 * is the owner getting informed if the instance owned should be released
	 * from memory (i.e. so that garbage collection can take place).
	 * 
	 * @param owner
	 *            the owner to be registered
	 */
	public void registerOwner(final IBitmapIdOwner owner);

	/**
	 * Gets the instance associated to the specified identifier from the cache.
	 * This method never returns {@code null} instead a new (mostly empty)
	 * instance is created.
	 * 
	 * @param id
	 *            the identifier of the cached instance to be retrieved
	 * 
	 * @return the instance for the specified identifier
	 */
	public T get(final K id);
}
