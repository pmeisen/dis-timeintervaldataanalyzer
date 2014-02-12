package net.meisen.dissertation.model.time.granularity;

import java.util.Date;


/**
 * A {@code TimeGranularity} which is based on the {@code Date}, i.e. a
 * representation can be retrieved from a {@code Date} instance by using
 * specific formats applied to the {@code Date}.
 * 
 * @author pmeisen
 * 
 */
public interface IDateBasedGranularity extends ITimeGranularity {

	/**
	 * Gets the formats used to extract the {@code long} values from the
	 * {@code Date}.
	 * 
	 * @return the formats used to extract the {@code long} values from the
	 *         {@code Date}
	 */
	public String[] getFormat();

	/**
	 * Determines the representer used for the extracted {@code long} values.
	 * 
	 * @param values
	 *            the extracted values
	 * 
	 * @return the representer
	 */
	public long determineRepresentor(final long[] values);

	/**
	 * Resolve a representer to a {@code Date} which can be used for output or
	 * formatting purposes.
	 * 
	 * @param value
	 *            the representer
	 *            
	 * @return the {@code Date} instance for the representer
	 */
	public Date resolveRepresenter(final long value);
}
