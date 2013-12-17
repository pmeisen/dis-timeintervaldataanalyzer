package net.meisen.dissertation.models.impl.time;

import net.meisen.dissertation.models.INaturals;

public class RawTimeInterval<T extends INaturals<T>> {
	private RawTimePoint<T> point;
	private T duration;

	public RawTimeInterval(final RawTimePoint<T> point, final T duration) {
		this.point = point;
		this.duration = duration;
	}

	public RawTimePoint<T> getStart() {
		return point;
	}

	public T getDuration() {
		return duration;
	}
	
	public RawTimePoint<T> getEnd() {
		T offset = point.getOffset();
		return new RawTimePoint<T>(offset.add(duration));
	}
}
