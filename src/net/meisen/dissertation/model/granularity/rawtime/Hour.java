package net.meisen.dissertation.model.granularity.rawtime;


/**
 * 60 minutes or 3,600 seconds
 */
public class Hour implements IRawTimeGranularity {

	@Override
	public boolean equals(final Object o) {
		if (o instanceof Hour) {
			return true;
		} else {
			return false;
		}
	}
}
