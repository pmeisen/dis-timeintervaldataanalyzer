package net.meisen.dissertation.model.cache;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.data.metadata.MetaDataCollection;
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
public interface IMetaDataCache {

	/**
	 * Caches the specified {@code MetaDataModel}.
	 * 
	 * @param model
	 *            the model to be cached
	 */
	public void cacheMetaDataModel(final MetaDataModel model);

	/**
	 * Creates a {@code MetaDataCollection} from the cached data, or if not
	 * available from the configuration (use the
	 * {@link DefaultValues#METADATACOLLECTION_ID} to retrieve the configured
	 * collection).
	 * 
	 * @return the cached {@code MetaDataCollection} or the configured one, if
	 *         none is cached
	 */
	public MetaDataCollection createMetaDataCollection();

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

	/**
	 * Initializes the {@code MetaDataCache} for the specified {@code model}.
	 * 
	 * @param model
	 *            the {@code TidaModel} to initialize the cache for
	 * 
	 * @see TidaModel
	 */
	public void initialize(final TidaModel model);

	/**
	 * Releases the {@code MetaDataCache} and all the resources bound.
	 */
	public void release();
}
