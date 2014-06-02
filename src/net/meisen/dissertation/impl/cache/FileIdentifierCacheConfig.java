package net.meisen.dissertation.impl.cache;

import java.io.File;

import net.meisen.dissertation.model.cache.IIdentifierCacheConfig;

/**
 * A {@code FileIdentifierCacheConfig} used to configure a
 * {@code FileIdentifierCache}.
 * 
 * @author pmeisen
 * 
 */
public class FileIdentifierCacheConfig implements IIdentifierCacheConfig {
	private File location;

	/**
	 * Creates a {@code FileIdentifierCacheConfig} used to configure a
	 * {@code FileIdentifierCache}.
	 */
	public FileIdentifierCacheConfig() {
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
