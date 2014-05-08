package net.meisen.dissertation.impl.cache;

import java.io.File;

import net.meisen.dissertation.model.cache.IBitmapCacheConfig;

/**
 * Configuration of the {@code FileCache}.
 * 
 * @author pmeisen
 * 
 */
public class FileCacheConfig implements IBitmapCacheConfig {
	private File location;

	/**
	 * Default constructor, configures the {@code FileCache} to use the default
	 * location defined by {@link FileCache#getDefaultLocation()}.
	 */
	public FileCacheConfig() {
		this((File) null);
	}

	/**
	 * Configures the {@code FileCache} to use the specified location.
	 * 
	 * @param location
	 *            the location to be used
	 */
	public FileCacheConfig(final String location) {
		this(location == null ? null : new File(location));
	}

	/**
	 * Configures the {@code FileCache} to use the specified location.
	 * 
	 * @param location
	 *            the location to be used
	 */
	public FileCacheConfig(final File location) {
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

}
