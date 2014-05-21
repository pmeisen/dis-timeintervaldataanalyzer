package net.meisen.dissertation.impl.cache;

import java.io.File;

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

public class FileMetaDataCache implements IMetaDataCache {

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

		}

		final MetaDataCollection metaDataCollection = new MetaDataCollection();

		for (final DescriptorModel<?> dm : model.getDescriptorModels()) {
			final String dmId = dm.getId();
			final LoadedMetaData metaData = new LoadedMetaData(dmId);

			// add the metaData
			for (final Descriptor<?, ?, ?> desc : dm.getDescriptors()) {
				final Object id = desc.getId();
				final Object value = desc.getValue();

				metaData.addValue(id, value);
			}

			// add the metaData
			metaDataCollection.addMetaData(metaData);
		}

		this.metaDataCollection = null;
	}

	@Override
	public MetaDataCollection createMetaDataCollection() {

		if (this.metaDataCollection == null) {

		}

		return metaDataCollection;
	}

	@Override
	public void release() {
		metaDataCollection.clear();
		metaDataCollection = null;

		init = false;
	}

	@Override
	public void setConfig(final IMetaDataCacheConfig config) {
		if (init) {

		}
	}

	@Override
	public void initialize(final TidaModel model) {
		if (metaDataCollection == null) {

		}

		init = true;
	}

	public File getLocation() {
		return null;
	}
}
