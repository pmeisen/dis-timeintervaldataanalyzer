package net.meisen.dissertation.model.time;

import net.meisen.dissertation.model.naturals.INaturals;

public class RawTimePoint<T extends INaturals<T>> {
	private T offset;

	public RawTimePoint(final T offset) {
		this.offset = offset;
	}

	public T getOffset() {
		return offset;
	}
}
