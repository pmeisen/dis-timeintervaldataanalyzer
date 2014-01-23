package net.meisen.dissertation.model.granularity.rawtime;


/**
 * 1 second
 */
public class Second implements IRawTimeGranularity {

	@Override
	public boolean equals(final Object o) {
		if (o instanceof Second) {
			return true;
		} else {
			return false;
		}
	}
}
