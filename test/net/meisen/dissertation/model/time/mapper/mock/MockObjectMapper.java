package net.meisen.dissertation.model.time.mapper.mock;

import net.meisen.dissertation.model.time.granularity.ITimeGranularity;
import net.meisen.dissertation.model.time.mapper.BaseMapper;

/**
 * A mock for a {@code Mapper}.
 * 
 * @author pmeisen
 * 
 */
public class MockObjectMapper extends BaseMapper<Object> {

	/**
	 * Just a mock with the default constructor
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
	public MockObjectMapper(final Object start, final Object end,
			final ITimeGranularity granularity) throws IllegalArgumentException {
		super(start, end, granularity);
	}

	@Override
	public Class<Object> getMappedType() {
		return Object.class;
	}

	@Override
	protected long map(final Object from) {
		return 0;
	}

	@Override
	public Object demap(final long value) {
		return null;
	}

	@Override
	protected Object resolve(final String value)
			throws IllegalArgumentException {
		return value;
	}
}
