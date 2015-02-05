package net.meisen.dissertation.impl.time.mapper;

import java.util.Date;

import net.meisen.dissertation.model.time.DateNormalizer;
import net.meisen.dissertation.model.time.DateNormalizer.RoundType;
import net.meisen.dissertation.model.time.granularity.ITimeGranularity;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.general.genmisc.types.Dates;

/**
 * A {@code DateMapper} is used to map {@code Date} instances to a representing
 * {@code Long} value.
 * 
 * @author pmeisen
 * 
 */
public class DateMapper extends BaseMapper<Date> {
	private final static DateNormalizer normalizer = DateNormalizer.instance();

	/**
	 * This constructor creates a {@code DateMapper} which maps data to
	 * representatives within the start and end and the specified
	 * {@code granularity}.
	 * 
	 * @param start
	 *            the minimal value for the {@code Mapper} (included)
	 * @param end
	 *            the maximum value for the {@code Mapper} (included)
	 * @param granularity
	 *            the granularity of the mapping, i.e. which information can get
	 *            truncated
	 * 
	 * @throws IllegalArgumentException
	 *             the exception is thrown in multiple cases if the {@code end}
	 *             value is smaller than the {@code start} value
	 */
	public DateMapper(final Date start, final Date end,
			final ITimeGranularity granularity) throws IllegalArgumentException {
		super(start, end, granularity);
	}

	@Override
	public Class<Date> getMappedType() {
		return Date.class;
	}

	@Override
	protected long map(final Date from) {
		return normalizer.normalize(from, getGranularity(), RoundType.FLOOR);
	}

	@Override
	public Date demap(final long value) {
		return normalizer.denormalize(value, getGranularity());
	}

	@Override
	public String toString() {

		// define the format by type
		final String format = "dd.MM.yyyy HH:mm:ss,SSS";
		return "Mapper for values between '"
				+ Dates.createStringFromDate(demap(getStart()), format)
				+ "' - '" + Dates.createStringFromDate(demap(getEnd()), format)
				+ "'";
	}

	@Override
	public String formatValue(final Date value) {
		return Dates.formatDate((Date) value, "dd.MM.yyyy HH:mm:ss,SSS");
	}

	@Override
	protected Date resolve(String value) {
		if (value == null) {
			return null;
		} else {
			final Date parsed = Dates.isDate(value, Dates.GENERAL_TIMEZONE);
			if (parsed == null) {
				throw new IllegalArgumentException("Value '" + value
						+ "' cannot be used as date.");
			} else {
				return parsed;
			}
		}
	}
}
