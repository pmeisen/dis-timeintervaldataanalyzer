package net.meisen.dissertation.model.granularity.rawtime;

import net.meisen.dissertation.models.IRawTimeGranularity;

/**
 * 0.1 seconds
 */
public class DeciSecond implements IRawTimeGranularity {

	@Override
	public boolean equals(final Object o) {
		if (o instanceof DeciSecond) {
			return true;
		} else {
			return false;
		}
	}
}
