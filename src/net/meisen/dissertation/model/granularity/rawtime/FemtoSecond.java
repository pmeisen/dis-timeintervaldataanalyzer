package net.meisen.dissertation.model.granularity.rawtime;

import net.meisen.dissertation.models.IRawTimeGranularity;

/**
 * 10^-15 seconds
 */
public class FemtoSecond implements IRawTimeGranularity {

	@Override
	public boolean equals(final Object o) {
		if (o instanceof FemtoSecond) {
			return true;
		} else {
			return false;
		}
	}
}
