package net.meisen.dissertation.impl.cache;

import net.meisen.dissertation.model.cache.IDataRecordCacheConfig;

/**
 * Configuration of a {@code DataRecordCache} based on a {@code mapDb}.
 * 
 * @author pmeisen
 * 
 */
public class MapDbDataRecordCacheConfig extends MapDbCacheConfig implements
		IDataRecordCacheConfig {
	// just an adapter to mark a default config as IDataRecordCacheConfig
}
