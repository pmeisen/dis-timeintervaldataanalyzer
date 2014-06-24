package net.meisen.dissertation.model.cache;

import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorModelSet;

/**
 * Interface which describes the requirements of a cache used to handle
 * {@code FactDescriptorModelSet} instances.
 * 
 * @author pmeisen
 * 
 */
public interface IFactDescriptorModelSetCache {

	/**
	 * Initializes the {@code FactDescriptorModelSetCache} for the specified
	 * {@code model}.
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
	public void setConfig(final IFactDescriptorModelSetCacheConfig configuration);

	/**
	 * Caches the specified {@code set} for the specified {@code bitmapId}.
	 * 
	 * @param bitmapId
	 *            the {@code BitmapId} of the {@code bitmap} to be cached
	 * @param set
	 *            the {@code FactDescriptorModelSet} to be cached
	 * 
	 * @see BitmapId
	 * @see FactDescriptorModelSet
	 */
	public void cacheFactDescriptorModelSet(final BitmapId<?> bitmapId,
			final FactDescriptorModelSet set);

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
