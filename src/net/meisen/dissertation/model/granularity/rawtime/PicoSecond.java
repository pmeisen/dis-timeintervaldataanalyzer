package net.meisen.dissertation.model.granularity.rawtime;


/**
 * 10^-12 seconds
 */
public class PicoSecond implements IRawTimeGranularity {

	@Override
	public boolean equals(final Object o) {
		if (o instanceof PicoSecond) {
			return true;
		} else {
			return false;
		}
	}
}
