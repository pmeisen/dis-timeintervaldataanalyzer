package net.meisen.dissertation.impl.cache;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.impl.data.metadata.DescriptorMetaDataCollection;
import net.meisen.dissertation.impl.data.metadata.ReadOnlyMetaDataCollection;
import net.meisen.dissertation.model.cache.IMetaDataCache;
import net.meisen.dissertation.model.cache.IMetaDataCacheConfig;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.data.metadata.IMetaDataCollection;
import net.meisen.dissertation.model.descriptors.Descriptor;
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
	private DescriptorMetaDataCollection metaDataCollection;

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	private boolean init = false;

	@Override
	public void initialize(final TidaModel model) {
		if (init) {
			return;
		}
		
		this.metaDataCollection = new DescriptorMetaDataCollection();
		init = true;
	}

	@Override
	public void cacheDescriptor(final Descriptor<?, ?, ?> desc) {
		if (!init) {
			exceptionRegistry.throwException(
					MemoryMetaDataCacheException.class, 1000);
		}

		metaDataCollection.addDescriptor(desc);
	}

	@Override
	public IMetaDataCollection createMetaDataCollection() {
		if (!init) {
			exceptionRegistry.throwException(
					MemoryMetaDataCacheException.class, 1000);
		}

		return new ReadOnlyMetaDataCollection(metaDataCollection);
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

	/**
	 * Checks if the {@code MemoryMetaDataCache} is initialized.
	 * 
	 * @return {@code true} if the cache is initialized, otherwise {@code false}
	 */
	public boolean isInitialized() {
		return init;
	}

	@Override
	public boolean setPersistency(final boolean enable) {
		return false;
	}

	@Override
	public void remove() {
		// nothing to do
	}
}
