package net.meisen.dissertation.model.granularity.rawtime;


/**
 * 10^-9 seconds
 */
public class NanoSecond implements IRawTimeGranularity {

	@Override
	public boolean equals(final Object o) {
		if (o instanceof NanoSecond) {
			return true;
		} else {
			return false;
		}
	}
}
