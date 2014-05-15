package net.meisen.dissertation.impl.cache;

import java.io.File;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.meisen.dissertation.model.cache.IBitmapCacheConfig;

/**
 * Configuration of the {@code FileCache}.
 * 
 * @author pmeisen
 * 
 */
public class FileCacheConfig implements IBitmapCacheConfig {
	private File location;
	private Integer cacheSize;
	private Double cacheCleaningFactor;
	private String maxFileSize;

	/**
	 * Creates a {@code FileCacheConfig} used to configure a {@code FileCache}.
	 */
	public FileCacheConfig() {
		setLocation(null);
		setCacheSize(null);
		setCacheCleaningFactor(null);
		setMaxFileSize(null);
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

	public Integer getCacheSize() {
		return cacheSize;
	}

	public void setCacheSize(final Integer cacheSize) {
		this.cacheSize = cacheSize;
	}

	public Double getCacheCleaningFactor() {
		return cacheCleaningFactor;
	}

	public void setCacheCleaningFactor(final Double cacheCleaningFactor) {
		this.cacheCleaningFactor = cacheCleaningFactor;
	}

	public Integer getMaxFileSizeInByte() {
		if (maxFileSize == null) {
			return null;
		} else if (maxFileSize.trim().isEmpty()) {
			return null;
		}

		final Pattern p = Pattern
				.compile("\\s*([0-9]*(?:\\.[0-9]+)?)\\s*([gmkb]?)\\s*");
		final Matcher m = p.matcher(maxFileSize.toLowerCase());

		if (m.matches()) {
			final String nr = m.group(1);
			final String unit = m.group(2);

			final double val = nr.isEmpty() ? 1.0 : Double.parseDouble(nr);
			final int factor;
			if ("g".equals(unit)) {
				factor = 1024 * 1024 * 1024;
			} else if ("k".equals(unit)) {
				factor = 1024;
			} else if ("b".equals(unit)) {
				factor = 1;
			} else {
				factor = 1024 * 1024;
			}

			final long res = (long) (val * factor);
			if (res > Integer.MAX_VALUE) {
				return Integer.MAX_VALUE;
			} else {
				return (int) res;
			}
		} else {
			return null;
		}
	}

	public String getMaxFileSize() {
		return maxFileSize;
	}

	public void setMaxFileSize(final String maxFileSize) {
		this.maxFileSize = maxFileSize;
	}
}
