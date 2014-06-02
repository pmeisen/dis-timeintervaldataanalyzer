package net.meisen.dissertation.model.cache;

import net.meisen.dissertation.impl.cache.BaseIdentifierCacheException;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;

/**
 * Cache used to cache the valid and last used identifiers.
 * 
 * @author pmeisen
 * 
 */
public interface IIdentifierCache {

	/**
	 * Initializes the cache for the specified {@code model}.
	 * 
	 * @param model
	 *            the {@code TidaModel} to initialize {@code this} for
	 * 
	 * @see TidaModel
	 */
	public void initialize(final TidaModel model);

	/**
	 * Get the last used identifier.
	 * 
	 * @return the last used identifier
	 */
	public int getLastUsedIdentifier();

	/**
	 * Gets a bitmap of the valid identifiers, i.e. those that are indexed
	 * completely and are not deleted.
	 * 
	 * @return a bitmap of the valid identifiers
	 */
	public Bitmap getValidIdentifiers();

	/**
	 * Marks the specified {@code identifier} as used. The marking has to be
	 * done in increasing order, i.e. if the method was called with a value, the
	 * next call must be done with a value larger the previous one
	 * 
	 * @param identifier
	 *            the identifier to be marked as used
	 * 
	 * @throws BaseIdentifierCacheException
	 *             if the value is not set in increasing order, or if the cache
	 *             is not initialized
	 */
	public void markIdentifierAsUsed(final int identifier)
			throws BaseIdentifierCacheException;

	/**
	 * Marks the identifiers as valid, i.e. to be used.
	 * 
	 * @param validIdentifier
	 *            the identifiers to be marked as used
	 */
	public void markIdentifierAsValid(final int... validIdentifier);

	/**
	 * Marks the specified as valid.
	 * 
	 * @param bitmap
	 *            the identifiers to be marked as valid
	 */
	public void markIdentifierAsValid(final Bitmap bitmap);

	/**
	 * Marks the identifiers as invalid, i.e. not to be used.
	 * 
	 * @param invalidIdentifier
	 *            the identifiers marked as not to be used
	 */
	public void markIdentifierAsInvalid(final int... invalidIdentifier);

	/**
	 * Sets the persistency of the cache, i.e. if values are persisted to the
	 * underlying persistence level (depending on the concrete
	 * {@code implementation} or not.
	 * 
	 * @param enable
	 *            {@code true} to enable persistency, otherwise {@code false}
	 * 
	 * @return the old value
	 */
	public boolean setPersistency(final boolean enable);

	/**
	 * Releases the cache and all bound resources. The cache should not be used
	 * anymore after releasing it.
	 */
	public void release();

	/**
	 * Sets the configuration of the cache. The method can only be called prior
	 * to initialization.
	 * 
	 * @param config
	 *            the configuration to be used
	 * 
	 * @throws BaseIdentifierCacheException
	 *             if the cache is initialized
	 */
	void setConfig(final IIdentifierCacheConfig config)
			throws BaseIdentifierCacheException;
}
