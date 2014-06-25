package net.meisen.dissertation.impl.cache;

import java.io.File;

/**
 * Configuration of a cache based on a {@code mapDb}.
 * 
 * @author pmeisen
 * 
 */
public class MapDbCacheConfig {
	private File location;
	private Integer cacheSize;
	private MapDbType type;

	/**
	 * Creates a {@code FileBitmapCacheConfig} used to configure a
	 * {@code FileBitmapCache}.
	 */
	public MapDbCacheConfig() {
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

	/**
	 * Gets the type of the {@code MapDb} to be used.
	 * 
	 * @return the type of the {@code MapDb} to be used
	 */
	public MapDbType getType() {
		return type;
	}

	/**
	 * Defines the type of the {@code MapDb} to be used.
	 * 
	 * @param type
	 *            the type of the {@code MapDb} to be used
	 */
	public void setType(final MapDbType type) {
		this.type = type == null ? MapDbType.getDefault() : type;
	}

	/**
	 * Defines the type of the {@code MapDb} to be used.
	 * 
	 * @param type
	 *            the type of the {@code MapDb} to be used
	 */
	public void setMapDbType(final String type) {
		this.type = MapDbType.find(type);
	}
}
