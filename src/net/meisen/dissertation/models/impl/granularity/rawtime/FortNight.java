package net.meisen.dissertation.models.impl.granularity.rawtime;

import net.meisen.dissertation.models.IRawTimeGranularity;

/**
 * 2 weeks, or 14 days
 */
public class FortNight implements IRawTimeGranularity {

	@Override
	public boolean equals(final Object o) {
		if (o instanceof FortNight) {
			return true;
		} else {
			return false;
		}
	}
}
