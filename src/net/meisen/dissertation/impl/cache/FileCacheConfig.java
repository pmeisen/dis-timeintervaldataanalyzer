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

	private Integer oldFactor;
	private Double timeThresholdFactor;
	private Double weightingTime;

	public FileCacheConfig() {
		setLocation(null);
		setTimeThresholdFactor(null);
		setWeightingTime(null);
	}

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

	public Integer getOldFactor() {
		return oldFactor;
	}

	public void setOldFactor(Integer oldFactor) {
		this.oldFactor = oldFactor;
	}

	public Double getTimeThresholdFactor() {
		return timeThresholdFactor;
	}

	public void setTimeThresholdFactor(final Double timeThresholdFactor) {
		this.timeThresholdFactor = timeThresholdFactor;
	}

	public Double getWeightingTime() {
		return weightingTime;
	}

	public void setWeightingTime(final Double weightingTime) {
		this.weightingTime = weightingTime;
	}
}
