package net.meisen.dissertation.model.time.mapper.mock;

import java.util.Date;

import net.meisen.dissertation.model.time.granularity.ITimeGranularity;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.general.genmisc.types.Dates;

/**
 * A mock for a {@code Mapper}.
 * 
 * @author pmeisen
 * 
 */
public class MockWithParametersMapper extends BaseMapper<Date> {

	/**
	 * Just a mock with the another constructor
	 * 
	 * @param start
	 *            the minimal value for the {@code Mapper} (included)
	 * @param end
	 *            the maximum value for the {@code Mapper} (included)
	 * @param granularity
	 *            the granularity of the mapping, i.e. which information can get
	 *            truncated
	 * @param object1
	 *            just a parameter
	 * @param object2
	 *            just a parameter
	 * 
	 * @throws IllegalStateException
	 *             throws always a {@code IllegalStateException}
	 */
	public MockWithParametersMapper(final Date start, final Date end,
			final ITimeGranularity granularity, final Object object1,
			final Object object2) throws IllegalStateException {
		super(start, end, granularity);

		// throw an exception
		throw new IllegalStateException("Don't use this constructor at all");
	}

	@Override
	public Class<Date> getMappedType() {
		return Date.class;
	}

	@Override
	protected long map(final Date from) {
		return 0;
	}

	@Override
	public Date demap(final long value) {
		return null;
	}

	@Override
	protected Date resolve(final String value) throws IllegalArgumentException {
		return Dates.isDate(value);
	}
}
