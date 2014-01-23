package net.meisen.dissertation.model.granularity.rawtime;

import net.meisen.dissertation.models.IRawTimeGranularity;

/**
 * 0.001 seconds
 */
public class MilliSecond implements IRawTimeGranularity {

	@Override
	public boolean equals(final Object o) {
		if (o instanceof MilliSecond) {
			return true;
		} else {
			return false;
		}
	}
}
