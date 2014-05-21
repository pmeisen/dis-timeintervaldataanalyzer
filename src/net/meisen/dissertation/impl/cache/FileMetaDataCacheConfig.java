package net.meisen.dissertation.impl.cache;

import java.io.File;

import net.meisen.dissertation.model.cache.IMetaDataCacheConfig;

/**
 * A configuration used for the {@code MetaDataCache}.
 * 
 * @author pmeisen
 * 
 */
public class FileMetaDataCacheConfig implements IMetaDataCacheConfig {
	private File location;

	/**
	 * Creates a {@code FileMetaDataCacheConfig} used to configure a
	 * {@code FileMetaDataCache}.
	 */
	public FileMetaDataCacheConfig() {
		setLocation(null);
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
}
