package net.meisen.dissertation.impl.time.granularity.mock;

import net.meisen.dissertation.model.time.granularity.ITimeGranularity;

/**
 * A valid {@code ITimeGranularity} offering a static instance method.
 * 
 * @author pmeisen
 * 
 */
public class MockValidTimeGranularityByInstance implements ITimeGranularity {

	/**
	 * New default constructor.
	 * 
	 * @param some
	 *            some value
	 */
	public MockValidTimeGranularityByInstance(int some) {
		// nothing to do
	}

	/**
	 * The valid instance method.
	 * 
	 * @return a new instance
	 */
	public static MockValidTimeGranularityByInstance instance() {
		return new MockValidTimeGranularityByInstance(0);
	}
}
