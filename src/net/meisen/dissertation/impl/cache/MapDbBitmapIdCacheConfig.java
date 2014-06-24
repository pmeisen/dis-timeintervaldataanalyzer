package net.meisen.dissertation.impl.cache;

import java.io.File;

import net.meisen.dissertation.model.cache.IBitmapIdCacheConfig;

public class MapDbBitmapIdCacheConfig implements IBitmapIdCacheConfig {
	private File location;
	private Integer cacheSize;
	private MapDbType type;

	/**
	 * Creates a {@code FileBitmapCacheConfig} used to configure a
	 * {@code FileBitmapCache}.
	 */
	public MapDbBitmapIdCacheConfig() {
		setLocation(null);
		setCacheSize(null);
		setType(null);
	}

	/**
	 * Sets the location.
	 * 
	 * @param location
	 *            the location to be used
	 */
	public void setLocation(final File location) {
		this.location = location;
	}

	/**
	 * Gets the location configured by {@code this}.
	 * 
	 * @return the location configured by {@code this}, can be {@code null}
	 */
	public File getLocation() {
		return location;
	}

	/**
	 * Gets the maximal amount of bitmaps to be cached.
	 * 
	 * @return the maximal amount of bitmaps to be cached
	 */
	public Integer getCacheSize() {
		return cacheSize;
	}

	/**
	 * Specifies the maximal amount of bitmaps to be cached.
	 * 
	 * @param cacheSize
	 *            the maximal amount of bitmaps to be cached
	 */
	public void setCacheSize(final Integer cacheSize) {
		this.cacheSize = cacheSize;
	}

	public MapDbType getType() {
		return type;
	}

	public void setType(final MapDbType type) {
		this.type = type == null ? MapDbType.getDefault() : type;
	}

	public void setMapDbType(final String type) {
		this.type = MapDbType.find(type);
	}
}
