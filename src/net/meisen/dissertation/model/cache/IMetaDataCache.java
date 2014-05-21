package net.meisen.dissertation.model.cache;

import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.data.metadata.MetaDataCollection;

public interface IMetaDataCache {

	public void cacheMetaDataModel(final MetaDataModel model);

	public MetaDataCollection createMetaDataCollection();
	
	public void setConfig(final IMetaDataCacheConfig config);
	
	public void initialize(final TidaModel model);
	
	public void release();
}
