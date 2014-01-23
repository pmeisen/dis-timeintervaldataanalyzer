package net.meisen.dissertation.model.granularity.rawtime;


/**
 * 0.01 seconds
 */
public class CentiSecond implements IRawTimeGranularity {

	@Override
	public boolean equals(final Object o) {
		if (o instanceof CentiSecond) {
			return true;
		} else {
			return false;
		}
	}
}
