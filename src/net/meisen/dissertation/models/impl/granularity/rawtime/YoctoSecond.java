package net.meisen.dissertation.models.impl.granularity.rawtime;

import net.meisen.dissertation.models.IRawTimeGranularity;

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
