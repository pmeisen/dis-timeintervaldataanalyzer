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
	 * Registers a {@code BitmapOwner} within the cache. The {@code BitmapOwner}
	 * is the own getting informed if the {@code Bitmap} should be released from
	 * memory (i.e. the garbage collection can take place).
	 * 
	 * @param owner
	 *            the owner to be registered
	 */
	public void registerOwner(final IBitmapIdOwner owner);

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
	 * Gets the {@code FactDescriptorModelSet} with the specified
	 * {@code bitmapId} from the cache. This method never returns {@code null}
	 * instead a new {@code FactDescriptorModelSet} should be created if no
	 * {@code FactDescriptorModelSet} is available with the specified
	 * {@code BitmapId}.
	 * 
	 * @param bitmapId
	 *            the {@code BitmapId} of the {@code Bitmap} to be retrieved
	 * 
	 * @return the {@code FactDescriptorModelSet} for the specified
	 *         {@code BitmapId}
	 */
	public FactDescriptorModelSet getSet(final BitmapId<?> bitmapId);

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
	 */
	public void setPersistency(final boolean enable);
}
