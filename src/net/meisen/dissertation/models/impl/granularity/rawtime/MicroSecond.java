package net.meisen.dissertation.models.impl.granularity.rawtime;

import net.meisen.dissertation.models.IRawTimeGranularity;

/**
 * 10^-6 seconds
 */
public class MicroSecond implements IRawTimeGranularity {

	@Override
	public boolean equals(final Object o) {
		if (o instanceof MicroSecond) {
			return true;
		} else {
			return false;
		}
	}
}
