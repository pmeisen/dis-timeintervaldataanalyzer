package net.meisen.dissertation.model.granularity.rawtime;


/**
 * 24 hours, 1,440 minutes or 86,400 seconds
 */
public class Day implements IRawTimeGranularity {

	@Override
	public boolean equals(final Object o) {
		if (o instanceof Day) {
			return true;
		} else {
			return false;
		}
	}
}
