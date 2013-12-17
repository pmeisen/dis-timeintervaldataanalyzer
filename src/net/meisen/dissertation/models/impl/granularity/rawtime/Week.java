package net.meisen.dissertation.models.impl.granularity.rawtime;

import net.meisen.dissertation.models.IRawTimeGranularity;

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
