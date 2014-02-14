package net.meisen.dissertation.model.time.granularity;

/**
 * Factory used to create instances of {@code TimeGranularity}.
 * 
 * @author pmeisen
 * 
 * @see ITimeGranularity
 * 
 */
public interface ITimeGranularityFactory {

	/**
	 * Creates an instance of a {@code TimeGranularity} based on the specified
	 * {@code timeGranularity}-string.
	 * 
	 * @param timeGranularity
	 *            the name or class of the {@code TimeGranularity} to be
	 *            returned
	 * @return the instance
	 * 
	 */
	public ITimeGranularity find(final String timeGranularity);

}
