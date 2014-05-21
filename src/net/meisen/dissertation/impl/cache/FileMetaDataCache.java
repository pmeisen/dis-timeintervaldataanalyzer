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

	private File location;
	private File modelLocation;

	@Override
	public void cacheMetaDataModel(final MetaDataModel model) {
		if (!init) {
			exceptionRegistry.throwException(FileMetaDataCacheException.class,
					1003);
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
		if (!init) {
			exceptionRegistry.throwException(FileMetaDataCacheException.class,
					1003);
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
			exceptionRegistry.throwException(FileMetaDataCacheException.class,
					1000);
		} else if (config == null) {
			this.location = null;
		} else if (config instanceof FileMetaDataCacheConfig) {
			final FileMetaDataCacheConfig fcc = (FileMetaDataCacheConfig) config;

			final File cLoc = fcc.getLocation();
			this.location = cLoc == null ? null : fcc.getLocation();
		} else {
			exceptionRegistry.throwException(FileMetaDataCacheException.class,
					1001, config.getClass().getName());
		}
	}

	@Override
	public void initialize(final TidaModel model) {

		// if already initialized we are done
		if (this.init) {
			return;
		}

		// read the values needed
		final String modelId = model.getId();
		final File modelLocation = model.getLocation();

		// determine the location of the model
		if (metaDataCollection == null) {
			exceptionRegistry.throwException(FileMetaDataCacheException.class,
					1002);
		} else if (this.location != null) {
			this.modelLocation = new File(location, modelId);
		} else if (modelLocation != null) {
			this.modelLocation = modelLocation;
			this.location = modelLocation.getParentFile();
		} else {
			this.location = getDefaultLocation();
			this.modelLocation = new File(getDefaultLocation(), modelId);
		}

		init = true;
	}

	/**
	 * Gets the default-location used by the {@code FileMetaDataCache} if no
	 * other is specified.
	 * 
	 * @return the default-location used by the {@code FileMetaDataCache}
	 */
	protected File getDefaultLocation() {
		return new File(".");
	}

	/**
	 * Gets the root-location of the caches. The {@code FileMetaDataCache}
	 * generates a sub-folder within this {@code location}.
	 * 
	 * @return the root-location of the caches
	 */
	public File getLocation() {
		return this.location;
	}

	/**
	 * Gets the location where the data of the cache is stored. This location is
	 * a sub-folder within the location specified by {@link #getLocation()}.
	 * 
	 * @return the location the cache stores data
	 */
	public File getModelLocation() {
		return this.modelLocation;
	}
}
