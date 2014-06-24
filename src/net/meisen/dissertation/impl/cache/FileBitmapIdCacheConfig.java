package net.meisen.dissertation.impl.cache;

import java.io.File;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.meisen.dissertation.model.cache.IBitmapIdCacheConfig;

/**
 * Configuration of the {@code FileBitmapCache}.
 * 
 * @author pmeisen
 * 
 */
public class FileBitmapIdCacheConfig implements IBitmapIdCacheConfig {
	private File location;
	private Integer cacheSize;
	private Double cacheCleaningFactor;
	private String maxFileSize;

	/**
	 * Creates a {@code FileBitmapCacheConfig} used to configure a
	 * {@code FileBitmapCache}.
	 */
	public FileBitmapIdCacheConfig() {
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
	 * Gets the factor of cleaning, i.e. the amount of bitmaps to be released
	 * when the cache is full.
	 * 
	 * @return the factor of cleaning
	 */
	public Double getCacheCleaningFactor() {
		return cacheCleaningFactor;
	}

	/**
	 * Sets the factor of cleaning, i.e. the amount of bitmaps to be released
	 * when the cache is full.
	 * 
	 * @param cacheCleaningFactor
	 *            the factor of cleaning
	 */
	public void setCacheCleaningFactor(final Double cacheCleaningFactor) {
		this.cacheCleaningFactor = cacheCleaningFactor;
	}

	/**
	 * Gets the maximal file-size in bytes used for a persisted bitmap-file.
	 * 
	 * @return the maximal file-size in bytes
	 */
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

	/**
	 * Gets the maximal file-size used for a persisted bitmap-file. The value
	 * can be specified by 1g (i.e. 1 gigabyte), m (for megabyte), k (for
	 * kilobyte) or b (for bytes).
	 * 
	 * @return the maximal file-size
	 */
	public String getMaxFileSize() {
		return maxFileSize;
	}

	/**
	 * Sets the maximal file-size used for a persisted bitmap-file. The value
	 * can be specified by 1g (i.e. 1 gigabyte), m (for megabyte), k (for
	 * kilobyte) or b (for bytes).
	 * 
	 * @param maxFileSize
	 *            the maximal file-size
	 */
	public void setMaxFileSize(final String maxFileSize) {
		this.maxFileSize = maxFileSize;
	}
}
