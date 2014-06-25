package net.meisen.dissertation.impl.cache;

import net.meisen.dissertation.model.cache.IBitmapIdCacheConfig;

/**
 * Configuration of a cache based on a {@code mapDb}.
 * 
 * @author pmeisen
 * 
 */
public class MapDbBitmapIdCacheConfig extends MapDbCacheConfig implements
		IBitmapIdCacheConfig {
	// just a adapter to mark a default config as IBitmapIdCacheConfig
}
