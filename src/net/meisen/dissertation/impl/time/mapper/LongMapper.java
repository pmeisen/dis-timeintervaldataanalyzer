package net.meisen.dissertation.impl.time.mapper;

import net.meisen.dissertation.model.time.granularity.ITimeGranularity;
import net.meisen.dissertation.model.time.mapper.BaseMapper;

/**
 * A {@code Mapper} to map long values.
 * 
 * @author pmeisen
 * 
 */
public class LongMapper extends BaseMapper<Long> {

	/**
	 * Generates a {@code Mapper} which is used to map values of the type
	 * {@code Long} to a {@code long}, {@code int}, {@code short}, or
	 * {@code byte} depending on the range of the values to map to (i.e.
	 * {@code end - start} ).
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
	public LongMapper(final long start, final long end,
			final ITimeGranularity granularity) throws IllegalArgumentException {
		super(start, end, granularity);
	}

	@Override
	public Class<Long> getMappedType() {
		return Long.class;
	}

	@Override
	protected long map(final Long from) {
		return from;
	}

	@Override
	protected Long validate(final Object o) {
		if (o instanceof Number) {
			return ((Number) o).longValue();
		} else {
			return super.validate(o);
		}
	}

	@Override
	public Long demap(final long value) {
		return value;
	}

	@Override
	public String toString() {
		return "Mapper for values between '" + demap(getStart()) + " "
				+ getGranularity() + "' - '" + demap(getEnd()) + " "
				+ getGranularity() + "'";
	}

	@Override
	protected Long resolve(final String value) {
		if (value == null) {
			return null;
		} else {
			try {
				return Long.parseLong(value);
			} catch (final NumberFormatException e) {
				throw new IllegalArgumentException("Value '" + value
						+ "' cannot be used as date.");
			}

		}
	}
}
