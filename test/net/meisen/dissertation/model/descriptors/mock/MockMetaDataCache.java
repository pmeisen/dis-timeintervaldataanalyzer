package net.meisen.dissertation.model.descriptors.mock;

import net.meisen.dissertation.model.cache.IMetaDataCache;
import net.meisen.dissertation.model.cache.IMetaDataCacheConfig;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.data.metadata.ConfiguredMetaDataCollection;
import net.meisen.dissertation.model.data.metadata.IMetaDataCollection;
import net.meisen.dissertation.model.descriptors.Descriptor;

/**
 * Just a mock of a {@code MetaDataCache}.
 * 
 * @author pmeisen
 * 
 */
public class MockMetaDataCache implements IMetaDataCache {

	@Override
	public boolean setPersistency(boolean enable) {
		return false;
	}

	@Override
	public void release() {
		// do nothing
	}

	@Override
	public void remove() {
		// do nothing
	}

	@Override
	public void initialize(final TidaModel model) {
		// do nothing
	}

	@Override
	public void cacheDescriptor(final Descriptor<?, ?, ?> desc) {
		// do nothing
	}

	@Override
	public IMetaDataCollection createMetaDataCollection() {
		return new ConfiguredMetaDataCollection();
	}

	@Override
	public void setConfig(final IMetaDataCacheConfig config) {
		// do nothing
	}
}
