package net.meisen.dissertation.impl.cache;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.impl.data.metadata.LoadedMetaData;
import net.meisen.dissertation.model.cache.IMetaDataCache;
import net.meisen.dissertation.model.cache.IMetaDataCacheConfig;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.data.metadata.MetaDataCollection;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * A {@code MetaDataCache} which keeps the data in memory only.
 * 
 * @author pmeisen
 * 
 */
public class MemoryMetaDataCache implements IMetaDataCache {

	@Autowired
	@Qualifier(DefaultValues.METADATACOLLECTION_ID)
	private MetaDataCollection metaDataCollection;

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	private boolean init = false;

	@Override
	public void cacheMetaDataModel(final MetaDataModel model) {
		if (!init) {
			exceptionRegistry.throwException(
					MemoryMetaDataCacheException.class, 1001);
		}

		// create the collection and cache it in memory
		this.metaDataCollection = createCollectionForModel(model);
	}

	/**
	 * Method used to create a {@code MetaDataCollection} for the specified
	 * {@code MetaDataModel}.
	 * 
	 * @param model
	 *            the {@code MetaDataModel} to create the
	 *            {@code MetaDataCollection} for
	 * 
	 * @return the created {@code MetaDataCollection}
	 */
	protected MetaDataCollection createCollectionForModel(
			final MetaDataModel model) {

		// create a new collection with the data of the model
		final MetaDataCollection metaDataCollection = new MetaDataCollection();
		for (final DescriptorModel<?> dm : model.getDescriptorModels()) {
			final String dmId = dm.getId();
			final LoadedMetaData metaData = new LoadedMetaData(dmId);

			// add the metaData
			for (final Descriptor<?, ?, ?> desc : dm.getAllDescriptors()) {
				final Object id = desc.getId();
				final Object value = desc.getValue();

				metaData.addValue(id, value);
			}

			// add the metaData
			metaDataCollection.addMetaData(metaData);
		}

		return metaDataCollection;
	}

	@Override
	public MetaDataCollection createMetaDataCollection() {
		if (!init) {
			exceptionRegistry.throwException(
					MemoryMetaDataCacheException.class, 1001);
		}

		return metaDataCollection;
	}

	@Override
	public void release() {
		if (metaDataCollection != null) {
			metaDataCollection.clear();
			metaDataCollection = null;
		}

		init = false;
	}

	@Override
	public void setConfig(final IMetaDataCacheConfig config) {
		// nothing can be configured
	}

	@Override
	public void initialize(final TidaModel model) {
		if (metaDataCollection == null) {
			exceptionRegistry.throwException(
					MemoryMetaDataCacheException.class, 1000);
		}

		init = true;
	}

	/**
	 * Checks if the {@code MemoryMetaDataCache} is initialized.
	 * 
	 * @return {@code true} if the cache is initialized, otherwise {@code false}
	 */
	public boolean isInitialized() {
		return init;
	}
}
