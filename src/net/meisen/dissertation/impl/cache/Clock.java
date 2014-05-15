package net.meisen.dissertation.impl.cache;

/**
 * Default implementation of the {@code Clock} used for by the
 * {@code CacheStrategy} to determine the current time.
 * 
 * @author pmeisen
 * 
 */
public class Clock {

	/**
	 * Gets the current time.
	 * 
	 * @return the current time
	 */
	public long getCurrentTime() {
		return System.currentTimeMillis();
	}
}
