package net.meisen.dissertation.model.granularity.rawtime;


/**
 * 10^-6 seconds
 */
public class MicroSecond implements IRawTimeGranularity {

	@Override
	public boolean equals(final Object o) {
		if (o instanceof MicroSecond) {
			return true;
		} else {
			return false;
		}
	}
}
