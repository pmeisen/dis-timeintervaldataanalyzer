package net.meisen.dissertation.model.time.mapper.mock;

import net.meisen.dissertation.model.time.granularity.ITimeGranularity;
import net.meisen.dissertation.model.time.granularity.MilliSecond;
import net.meisen.dissertation.model.time.mapper.BaseMapper;

/**
 * A mock for a {@code Mapper}.
 * 
 * @author pmeisen
 * 
 */
public class MockSeveralConstructorsMapper extends BaseMapper<Object> {

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
	 * @param object
	 *            just a parameter
	 */
	public MockSeveralConstructorsMapper(final Object start, final Object end,
			final ITimeGranularity granularity, final Object object) {
		super(start, end, granularity);
	}

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
	 */
	public MockSeveralConstructorsMapper(final Object start, final Object end,
			final ITimeGranularity granularity, final Object object1,
			final Object object2) {
		super(start, end, granularity);
	}

	/**
	 * Just a mock with the another constructor
	 * 
	 * @param start
	 *            the minimal value for the {@code Mapper} (included)
	 * @param end
	 *            the maximum value for the {@code Mapper} (included)
	 */
	public MockSeveralConstructorsMapper(final Object start, final Object end) {
		super(start, end, MilliSecond.instance());
	}

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
	 */
	public MockSeveralConstructorsMapper(final Object start, final Object end,
			final ITimeGranularity granularity) {
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
