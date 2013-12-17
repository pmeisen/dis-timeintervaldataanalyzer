package net.meisen.dissertation.models.impl.granularity.rawtime;

import net.meisen.dissertation.models.IRawTimeGranularity;

/**
 * 10^-21 seconds
 */
public class ZeptoSecond implements IRawTimeGranularity {

	@Override
	public boolean equals(final Object o) {
		if (o instanceof ZeptoSecond) {
			return true;
		} else {
			return false;
		}
	}
}
