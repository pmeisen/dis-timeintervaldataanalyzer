package net.meisen.dissertation.impl.time.mapper.mock;

import net.meisen.dissertation.model.time.granularity.ITimeGranularity;
import net.meisen.dissertation.model.time.mapper.BaseMapper;

/**
 * Simply mocked mapper.
 * 
 * @author pmeisen
 * 
 */
public class MockMapper1 extends BaseMapper<Integer> {

	/**
	 * Default constructor.
	 * 
	 * @param start
	 *            the minimal value for the {@code Mapper} (included)
	 * @param end
	 *            the maximum value for the {@code Mapper} (included)
	 * @param granularity
	 *            the granularity of the mapping, i.e. which information can get
	 *            truncated
	 * @param params
	 *            parameters which are ignored by the default implementation
	 */
	public MockMapper1(final Integer start, final Integer end,
			final ITimeGranularity granularity, final Object[] params) {
		super(start, end, granularity, params);
	}

	@Override
	public Class<Integer> getMappedType() {
		return Integer.class;
	}

	@Override
	protected long map(final Integer from) {
		return 0;
	}

	@Override
	public Integer demap(final long value) {
		return (int) value;
	}

	@Override
	protected Integer resolve(final String value)
			throws IllegalArgumentException {
		return Integer.parseInt(value);
	}
}
