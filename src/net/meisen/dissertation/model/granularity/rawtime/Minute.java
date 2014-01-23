package net.meisen.dissertation.model.granularity.rawtime;


/**
 * 60 seconds
 */
public class Minute implements IRawTimeGranularity {

	@Override
	public boolean equals(final Object o) {
		if (o instanceof Minute) {
			return true;
		} else {
			return false;
		}
	}
}
