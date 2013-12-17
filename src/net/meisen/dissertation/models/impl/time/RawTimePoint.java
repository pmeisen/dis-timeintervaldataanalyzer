package net.meisen.dissertation.models.impl.time;

import net.meisen.dissertation.models.INaturals;

public class RawTimePoint<T extends INaturals<T>> {
	private T offset;

	public RawTimePoint(final T offset) {
		this.offset = offset;
	}

	public T getOffset() {
		return offset;
	}
}
