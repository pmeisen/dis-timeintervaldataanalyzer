package net.meisen.dissertation.model.granularity.rawtime;


/**
 * 7 days
 */
public class Week implements IRawTimeGranularity {

	@Override
	public boolean equals(final Object o) {
		if (o instanceof Week) {
			return true;
		} else {
			return false;
		}
	}
}
