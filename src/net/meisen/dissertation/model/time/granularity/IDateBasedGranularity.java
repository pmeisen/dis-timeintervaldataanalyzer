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
	 * Helper with some default implementations for concrete implementations of
	 * {@link IDateBasedGranularity}.
	 * 
	 * @author pmeisen
	 * 
	 */
	public static class DateBasedHelper {

		/**
		 * Helper method to retrieve the formats (specified by the
		 * {@code formats}) from the {@code date}.
		 * 
		 * @param formats
		 *            the formats used to extract the values
		 * @param date
		 *            the {@code Date} to retrieve the formats from
		 * 
		 * @return the retrieved values
		 * 
		 * @see IDateBasedGranularity
		 */
		public long[] getFormats(final DateFormat[] formats, final Date date) {
			final int amountOfFormats = formats.length;

			// get the values for the formats
			final long[] values = new long[amountOfFormats];
			for (int i = 0; i < amountOfFormats; i++) {
				final DateFormat format = formats[i];

				// set the values
				values[i] = format.getValue(date);
			}

			return values;
		}
	}

	/**
	 * An instance of the {@code DateBasedGranularity} default implementation.
	 * Within a concrete implementation of a {@code IDateBasedGranularity} the
	 * static should be accessed via a getter method to ensure overloading
	 * capabilities. <br/>
	 * 
	 * <pre>
	 * public DateBasedHelper helper() {
	 * 	return HELPER;
	 * }
	 * </pre>
	 */
	public final static DateBasedHelper helper = new DateBasedHelper();

	/**
	 * Gets the format of the date which represents the
	 * {@code DateBasedGranularity}.
	 * 
	 * @return the {@code DateFormat} which represents the {@ode
	 *         DateBasedGranularity}
	 */
	public DateFormat getFormat();

	/**
	 * Determines the representer used for the extracted {@code long} values.
	 * There is a default implementation available
	 * {@link DateBasedHelper#getFormats(DateFormat[], Date)}, which can be used
	 * to extract the values from the date to calculate the representor.
	 * 
	 * @param date
	 *            the date to get the representor for
	 * 
	 * @return the representer
	 */
	public long determineRepresentor(final Date date);

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
