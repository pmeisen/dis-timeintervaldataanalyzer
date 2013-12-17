package net.meisen.dissertation.models.impl.time;

import net.meisen.dissertation.models.INaturals;
import net.meisen.dissertation.models.INaturalsFactory;

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
