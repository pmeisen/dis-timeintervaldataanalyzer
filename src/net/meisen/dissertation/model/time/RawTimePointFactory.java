package net.meisen.dissertation.model.time;

import net.meisen.dissertation.model.naturals.INaturals;
import net.meisen.dissertation.model.naturals.INaturalsFactory;

public class RawTimePointFactory<T extends INaturals<T>> {

	private INaturalsFactory<T> factory;

	public RawTimePointFactory(final INaturalsFactory<T> factory) {
		this.factory = factory;
	}

	public RawTimePoint<T> createPoint(final T offset) {
		return new RawTimePoint<T>(offset);
	}

	public RawTimePoint<T> createPoint(final Object offset) {
		return createPoint(factory.generate(offset));
	}

	public RawTimePoint<T> createRandomPoint() {
		return createPoint(this.factory.getRandom());
	}
}
