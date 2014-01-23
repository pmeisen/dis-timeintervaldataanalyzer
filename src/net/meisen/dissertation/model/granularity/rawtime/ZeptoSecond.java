package net.meisen.dissertation.model.granularity.rawtime;


/**
 * 10^-21 seconds
 */
public class ZeptoSecond implements IRawTimeGranularity {

	@Override
	public boolean equals(final Object o) {
		if (o instanceof ZeptoSecond) {
			return true;
		} else {
			return false;
		}
	}
}
