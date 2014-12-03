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
		private final String timeDateIdentifiers = "ymdhnsi";

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

		/**
		 * Helper method to determine if the {@code thisIdentifier} is
		 * assignable to the {@code fitsIntoIdentifier}. The identifiers are one
		 * of:
		 * <ul>
		 * <li>{@code y} - year</li>
		 * <li>{@code m} - month</li>
		 * <li>{@code d} - day</li>
		 * <li>{@code h} - hour</li>
		 * <li>{@code n} - minute</li>
		 * <li>{@code s} - second</li>
		 * <li>{@code i} - millisecond</li>
		 * </ul>
		 * 
		 * @param thisIdentifier
		 *            the identifier to be checked if it fits into the
		 *            {@code fitsIntoIdentifier}
		 * @param assignableToIdentifier
		 *            the identifier to be checked if it fits into the
		 *            {@code thisIdentifier}
		 * 
		 * @return {@code true} if {@code thisIdentifier} fits into the
		 *         specified {@code fitsIntoIdentifier}, otherwise {@code false}
		 */
		public boolean isAssignableTo(final char thisIdentifier,
				final char assignableToIdentifier) {
			if (thisIdentifier == assignableToIdentifier) {
				return true;
			} else if (assignableToIdentifier == 'i') {
				return false;
			} else {
				final int thisPos = timeDateIdentifiers.indexOf(thisIdentifier);
				final int assignableToPos = timeDateIdentifiers
						.indexOf(assignableToIdentifier);
				return thisPos > assignableToPos;
			}
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
	 * @return the {@code DateFormat} which represents the
	 *         {@code DateBasedGranularity}
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

	/**
	 * This method is used to determine if {@code this} is assignable to a
	 * specific part of a date-value specified by the {@code identifier}. That
	 * means, e.g. a day is assignable to a specific year, i.e.
	 * {@code day.isAssignableTo('y')} returns {@code true}.
	 * 
	 * @param identifier
	 *            one of the identifiers:
	 *            <ul>
	 *            <li>{@code y} - year</li>
	 *            <li>{@code m} - month</li>
	 *            <li>{@code d} - day</li>
	 *            <li>{@code h} - hour</li>
	 *            <li>{@code n} - minute</li>
	 *            <li>{@code s} - second</li>
	 *            <li>{@code i} - millisecond</li>
	 *            </ul>
	 * 
	 * @return {@code true} if {@code this} fits into the specified
	 *         {@code identifier}, otherwise {@code false}
	 */
	public boolean isAssignableTo(final char identifier);

	/**
	 * Gets the identifier of {@code this}. The identifier defines what level of granularity is used by {@code this}.
	 * 
	 * @return one of the identifiers:
	 *            <ul>
	 *            <li>{@code y} - year</li>
	 *            <li>{@code m} - month</li>
	 *            <li>{@code d} - day</li>
	 *            <li>{@code h} - hour</li>
	 *            <li>{@code n} - minute</li>
	 *            <li>{@code s} - second</li>
	 *            <li>{@code i} - millisecond</li>
	 *            </ul>
	 * 
	 * @see #isAssignableTo(char)
	 */
	public char getIdentifier();
}
