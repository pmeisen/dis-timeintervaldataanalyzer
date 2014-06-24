package net.meisen.dissertation.model.cache;

/**
 * The reference-mechanism should be used for caches, which cannot trigger the
 * releasing of an instance from memory. The users of the bitmap ensure, that
 * the bitmap is only kept as weak-reference, and reloaded if needed.
 * 
 * @author pmeisen
 * 
 * @param <K>
 *            the identifier of the cached instance
 * @param <T>
 *            the instance to be cached
 */
public interface IReferenceMechanismCache<K, T> {

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
