package net.meisen.dissertation.model.time.mapper.mock;

import net.meisen.dissertation.model.time.granularity.ITimeGranularity;
import net.meisen.dissertation.model.time.mapper.BaseMapper;

/**
 * A mock for a {@code Mapper}.
 * 
 * @author pmeisen
 * 
 */
public class MockJustStartEndMapper extends BaseMapper<Integer> {

	/**
	 * Just a mock with the default constructor
	 * 
	 * @param start
	 *            the minimal value for the {@code Mapper} (included)
	 * @param end
	 *            the maximum value for the {@code Mapper} (included)
	 * @param granularity
	 *            the granularity to be picked
	 */
	public MockJustStartEndMapper(final int start, final int end,
			final ITimeGranularity granularity) {
		super(start, end, granularity);
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
		return 0;
	}

	@Override
	protected Integer resolve(final String value)
			throws IllegalArgumentException {
		return Integer.parseInt(value);
	}
}
