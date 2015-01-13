package net.meisen.dissertation.model.cache;

import net.meisen.dissertation.model.data.TidaModel;

/**
 * Cache used to cache the valid and last used identifiers.
 * 
 * @author pmeisen
 * 
 */
public interface ICache {

	/**
	 * Method used to disable or enable (default) the persistence. During the
	 * time the persistence is disabled all changes are just done within the
	 * memory. Whenever the persistence is enabled again, all data have to be
	 * persisted if supported by the cache.
	 * 
	 * @param enable
	 *            {@code true} to enable persistence, otherwise {@code false}
	 * 
	 * @return the old value of the persistence setting
	 */
	public boolean setPersistency(final boolean enable);

	/**
	 * Releases the cache and all bound resources. The cache should not be used
	 * anymore after releasing it.
	 */
	public void release();

	/**
	 * Method used to permanently remove the cache and all data persisted by it.
	 */
	public void remove();

	/**
	 * Initializes the cache for the specified {@code model}.
	 * 
	 * @param model
	 *            the {@code TidaModel} to initialize {@code this} for
	 * 
	 * @see TidaModel
	 */
	public void initialize(final TidaModel model);
}
