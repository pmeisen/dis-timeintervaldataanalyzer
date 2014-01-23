package net.meisen.dissertation.model.granularity.rawtime;


/**
 * 10^-24 seconds
 */
public class YoctoSecond implements IRawTimeGranularity {

	@Override
	public boolean equals(final Object o) {
		if (o instanceof YoctoSecond) {
			return true;
		} else {
			return false;
		}
	}
}
