package net.meisen.dissertation.model.time;

import java.util.Date;

import net.meisen.dissertation.model.time.granularity.IDateBasedGranularity;
import net.meisen.dissertation.model.time.granularity.ISecondBasedGranularity;
import net.meisen.dissertation.model.time.granularity.ITimeGranularity;
import net.meisen.dissertation.model.time.granularity.MilliSecond;
import net.meisen.general.genmisc.types.Dates;
import net.meisen.general.genmisc.types.Numbers;

/**
 * Helper class to normalize dates to long values.
 * 
 * @author pmeisen
 * 
 */
public class DateNormalizer {
	private final static DateNormalizer normalizer = new DateNormalizer();

	/**
	 * Defines the types of rounding, which might be needed by the concrete
	 * implementations.
	 * 
	 * @author pmeisen
	 * 
	 */
	public static enum RoundType {
		/**
		 * Round to the ceil
		 */
		CEIL,
		/**
		 * Round to the floor
		 */
		FLOOR,
		/**
		 * Round mathematically
		 */
		ROUND;
	}

	private final boolean supportsLeapSeconds;

	/**
	 * Gets the instance of a {@code DateNormalizer}.
	 * 
	 * @return the instance
	 */
	public static DateNormalizer instance() {
		return normalizer;
	}

	/**
	 * Default constructor.
	 */
	private DateNormalizer() {

		// check for leap-seconds
		supportsLeapSeconds = getLeapSeconds(new Date()) > 0;
	}

	/**
	 * Normalizes the specified {@code date} to the specified
	 * {@code granularity} using the specified {@code roundType}.
	 * 
	 * @param date
	 *            the date to be normalized, cannot be {@code null}
	 * @param granularity
	 *            the {@code TimeGranularity}
	 * @param roundType
	 *            the way of rounding the result
	 * 
	 * @return the normalized value
	 * 
	 * @see ITimeGranularity if the
	 */
	public long normalize(final Date date, final ITimeGranularity granularity,
			final RoundType roundType) {
		if (date == null) {
			throw new NullPointerException("The date cannot be null.");
		}

		if (isSecondBased(granularity)) {
			return normalizeSecondBased(date, roundType,
					(ISecondBasedGranularity) granularity);
		} else if (isDateBased(granularity)) {
			return normalizeDateBased(date, roundType,
					(IDateBasedGranularity) granularity);
		} else {
			return date.getTime();
		}
	}

	/**
	 * Denormalizes a normalized value back to a {@code date}.
	 * 
	 * @param value
	 *            the normalized value to be denormalized
	 * @param granularity
	 *            the {@code TimeGranularity} used to normalize the value
	 * 
	 * @return the {@code Date} retrieved for the normalized value
	 */
	public Date denormalize(final long value, final ITimeGranularity granularity) {

		if (isSecondBased(granularity)) {
			final double m = getMultiplier(
					(ISecondBasedGranularity) granularity,
					MilliSecond.instance());
			return new Date(Math.round(value * m));
		} else if (isDateBased(granularity)) {
			return ((IDateBasedGranularity) granularity)
					.resolveRepresenter(value);
		} else {
			return new Date(value);
		}
	}

	/**
	 * Normalizes a {@code DataBasedGranularity}.
	 * 
	 * @param date
	 *            the date to be normalized, cannot be {@code null}
	 * @param granularity
	 *            the {@code TimeGranularity}
	 * @param roundType
	 *            the way of rounding the result
	 * 
	 * @return the normalized value
	 * 
	 * @see IDateBasedGranularity
	 */
	protected long normalizeDateBased(final Date date,
			final RoundType roundType, final IDateBasedGranularity granularity) {
		return granularity.determineRepresentor(date);
	}

	/**
	 * Normalizes a {@code SecondBasedGranularity}.
	 * 
	 * @param date
	 *            the date to be normalized, cannot be {@code null}
	 * @param granularity
	 *            the {@code TimeGranularity}
	 * @param roundType
	 *            the way of rounding the result
	 * 
	 * @return the normalized value
	 * 
	 * @see ISecondBasedGranularity
	 */
	protected long normalizeSecondBased(final Date date,
			final RoundType roundType, final ISecondBasedGranularity granularity) {

		// the time in milliseconds
		long time = date.getTime();

		if (supportsLeapSeconds) {
			time = time - getLeapSeconds(date);
		}

		// get the multiplier to map the MilliSeconds to the granularity
		final double multiplier = getMultiplier(MilliSecond.instance(),
				granularity);

		// get the result and make sure it fits
		double res = time * multiplier;
		if (res > Long.MAX_VALUE) {
			throw new IllegalArgumentException("The date '"
					+ Dates.formatDate(date, "dd.MM.yyyy HH:mm:ss,SSS",
							Dates.GENERAL_TIMEZONE)
					+ "' cannot be normalized to '" + granularity + "'.");
		} else if (RoundType.CEIL.equals(roundType)) {
			res = Math.ceil(res);
		} else if (RoundType.FLOOR.equals(roundType)) {
			res = Math.floor(res);
		}

		return Math.round(res);
	}

	/**
	 * Checks if the {@code moreDetailed} granularity is more detailed than the
	 * {@code base}. A granularity is considered to be more detailed if and only
	 * if the resolution of the {@code base} granularity would lead to several
	 * instances of the {@code moreDetailed} granularity.
	 * 
	 * @param moreDetailed
	 *            the granularity to be checked if it is more detailed than the
	 *            {@code base} one
	 * @param base
	 *            the granularity to be checked to be more general than the
	 *            {@code moreDetailed} one
	 * 
	 * @return {@code true} if {@code moreDetailed} is more detailed than the
	 *         {@code base}, otherwise {@code false}
	 */
	public boolean isMoreDetailed(final ITimeGranularity moreDetailed,
			final ITimeGranularity base) {

		if (isSecondBased(moreDetailed) && isSecondBased(base)) {
			return compare((ISecondBasedGranularity) moreDetailed,
					(ISecondBasedGranularity) base) < 0;
		} else {
			return false;
		}
	}

	/**
	 * Checks if the specified {@code granularity} is a {@code SecondBased} one.
	 * 
	 * @param granularity
	 *            the {@code TimeGranularity} to be checked
	 * 
	 * @return {@code true} if the {@code TimeGranularity} is
	 *         {@code SecondBased}, otherwise {@code false}
	 * 
	 * @see ISecondBasedGranularity
	 */
	public boolean isSecondBased(final ITimeGranularity granularity) {
		return granularity instanceof ISecondBasedGranularity;
	}

	/**
	 * Checks if the specified {@code granularity} is a {@code DateBased} one.
	 * 
	 * @param granularity
	 *            the {@code TimeGranularity} to be checked
	 * 
	 * @return {@code true} if the {@code TimeGranularity} is {@code DateBased},
	 *         otherwise {@code false}
	 * 
	 * @see IDateBasedGranularity
	 */
	public boolean isDateBased(final ITimeGranularity granularity) {
		return granularity instanceof IDateBasedGranularity;
	}

	/**
	 * Compares the two {@code TimeHierarchicalGranularity} instances.
	 * 
	 * @param g1
	 *            the {@code TimeHierarchicalGranularity} to compare with
	 *            {@code g2}
	 * @param g2
	 *            the {@code TimeHierarchicalGranularity} to compare with
	 *            {@code g1}
	 * 
	 * @return {@code 1} if {@code g1 > g2}, {@code -1} if {@code g1 < g2}, and
	 *         {@code 0} if {@code g1 == g2}
	 * 
	 * @see ISecondBasedGranularity
	 */
	protected int compare(final ISecondBasedGranularity g1,
			final ISecondBasedGranularity g2) {
		final int secG1 = g1.seconds();
		final int secG2 = g2.seconds();
		final int expG1 = g1.expFractionOfSeconds();
		final int expG2 = g2.expFractionOfSeconds();

		final int result;
		if (secG1 < 0 && secG2 < 0) {

			// compare the exponents
			if (expG1 < expG2) {
				result = 1;
			} else if (expG1 > expG2) {
				result = -1;
			} else {
				result = 0;
			}
		} else if (secG1 < 0) {
			result = -1;
		} else if (secG2 < 0) {
			result = 1;
		} else {

			// compare the seconds
			if (secG1 < secG2) {
				result = -1;
			} else if (secG1 > secG2) {
				result = 1;
			} else {
				result = 0;
			}
		}

		return result;
	}

	/**
	 * Transforms the specified {@code date} by adding the {@code duration} to
	 * it. The {@code duration} is thereby defined in the {@code from}
	 * -granularity. The specified {@code roundType} is used in case rounding is
	 * needed.
	 * 
	 * @param date
	 *            the {@code Date} to add the duration to
	 * @param duration
	 *            the duration to be transformed
	 * @param durationGranularity
	 *            the granularity the duration is specified in
	 * @param roundType
	 *            the rounding type if rounding is needed
	 * 
	 * @return the transformed {@code Date} in the {@code to}-granularity
	 */
	public Date addDuration(final Date date, final long duration,
			final ITimeGranularity durationGranularity,
			final RoundType roundType) {

		if (durationGranularity == null) {
			throw new NullPointerException(
					"Cannot transform any value if from is null.");
		} else if (isSecondBased(durationGranularity)) {
			final ISecondBasedGranularity g = (ISecondBasedGranularity) durationGranularity;
			final double m = getMultiplier(g, MilliSecond.instance());

			// calculate the result
			double res = m * duration;		
			if (RoundType.CEIL.equals(roundType)) {
				res = Math.ceil(res);
			} else if (RoundType.FLOOR.equals(roundType)) {
				res = Math.floor(res);
			}

			return new Date(date.getTime() + Math.round(res));
		} else if (isDateBased(durationGranularity)) {
			final IDateBasedGranularity g = (IDateBasedGranularity) durationGranularity;
			return g.getFormat().modify(date, duration);
		} else {
			return date;
		}
	}

	/**
	 * Calculates the multiplier needed to transform a value of the {@code from}
	 * -granularity to the {@code to}-granularity.
	 * 
	 * @param from
	 *            the granularity to transform from
	 * @param to
	 *            the granularity to transform to
	 * 
	 * @return the multiplier used to multiply a value of {@code from} with to
	 *         receive a value of {@code to}
	 */
	protected double getMultiplier(final ISecondBasedGranularity from,
			final ISecondBasedGranularity to) {

		if (!isSecondBased(from) || !isSecondBased(to)) {
			throw new IllegalArgumentException(
					"Please use a 'timeHierarical-granularity' (use isTimeHierarchical(ITimeGranularity)).");
		} else if (from.equals(to)) {
			return 1.0;
		} else {

			// cast
			final ISecondBasedGranularity f = ((ISecondBasedGranularity) from);
			final ISecondBasedGranularity t = ((ISecondBasedGranularity) to);

			// get the values
			final int secFrom = f.seconds();
			final int secTo = t.seconds();
			final int expFrom = f.expFractionOfSeconds();
			final int expTo = t.expFractionOfSeconds();

			// calculate
			if (secFrom < 0 && secTo < 0) {
				return Math.pow(10, expTo - expFrom);
			} else if (secFrom < 0) {
				return 1.0 / (Math.pow(10, expFrom) * (double) secTo);
			} else if (secTo < 0) {
				return (double) secFrom / Math.pow(10, -1 * expTo);
			} else {
				return (double) secFrom / (double) secTo;
			}
		}
	}

	/**
	 * Gets the leap seconds added until the specified {@code date}.
	 * 
	 * @param date
	 *            the date until the leap seconds should be counted
	 * 
	 * @return the leap seconds added so far within the specified {@code date}
	 */
	protected int getLeapSeconds(final Date date) {
		final Date truncDate = Dates.truncateDate(date);
		return Numbers.castToInt(truncDate.getTime() % 60000);
	}
}
