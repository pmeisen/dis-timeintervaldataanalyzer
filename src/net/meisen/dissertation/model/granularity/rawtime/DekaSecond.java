package net.meisen.dissertation.model.granularity.rawtime;

import net.meisen.dissertation.models.IRawTimeGranularity;

/**
 * 10 seconds
 */
public class DekaSecond implements IRawTimeGranularity {

	@Override
	public boolean equals(final Object o) {
		if (o instanceof DekaSecond) {
			return true;
		} else {
			return false;
		}
	}
}
