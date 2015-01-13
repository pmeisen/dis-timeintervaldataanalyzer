package net.meisen.dissertation.model.cache;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.metadata.IMetaDataCollection;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;

/**
 * A {@code MetaDataCache} is a cache for the data held within a
 * {@code MetaDataModel}, i.e. {@code DescriptorModel} and {@code Descriptor}.
 * 
 * @see MetaDataModel
 * @see DescriptorModel
 * @see Descriptor
 * 
 * @author pmeisen
 * 
 */
public interface IMetaDataCache extends ICache {

	/**
	 * Caches the specified descriptor as part of the meta-data.
	 * 
	 * @param desc
	 *            the descriptor to be cached
	 */
	public void cacheDescriptor(final Descriptor<?, ?, ?> desc);

	/**
	 * Creates a {@code MetaDataCollection} from the cached data, and the
	 * configuration (use the {@link DefaultValues#METADATACOLLECTION_ID} to
	 * retrieve the configured collection).
	 * 
	 * @return the cached {@code MetaDataCollection} or the configured one, if
	 *         none is cached
	 * 
	 * @see IMetaDataCollection
	 */
	public IMetaDataCollection createMetaDataCollection();

	/**
	 * Specifies the configuration to be used by the concrete implementation.
	 * Mostly the concrete implementation only supports a concrete, none-general
	 * {@code MetaDataCacheConfig}
	 * 
	 * @param config
	 *            the configuration to be used, can be {@code null} so that a
	 *            default configuration should be used
	 * 
	 * @see IMetaDataCacheConfig
	 */
	public void setConfig(final IMetaDataCacheConfig config);
}
