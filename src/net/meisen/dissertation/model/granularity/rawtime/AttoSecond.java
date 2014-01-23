package net.meisen.dissertation.model.granularity.rawtime;

import net.meisen.dissertation.models.IRawTimeGranularity;

/**
 * 10^-18 seconds
 */
public class AttoSecond implements IRawTimeGranularity {

	@Override
	public boolean equals(final Object o) {
		if (o instanceof AttoSecond) {
			return true;
		} else {
			return false;
		}
	}
}
