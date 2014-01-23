package net.meisen.dissertation.model.time;

import net.meisen.dissertation.model.naturals.INaturals;
import net.meisen.dissertation.model.naturals.INaturalsFactory;

public class RawTimeIntervalFactory<T extends INaturals<T>> {
	private INaturalsFactory<T> naturalsFactory;
	private RawTimePointFactory<T> rawPointFactory;

	public RawTimeIntervalFactory(final INaturalsFactory<T> factory) {
		this.rawPointFactory = new RawTimePointFactory<T>(factory);
		this.naturalsFactory = factory;
	}

	public Class<T> getNaturalsType() {
		return this.naturalsFactory.getNaturalsType();
	}

	public Class<?> getJavaType() {
		return this.naturalsFactory.getJavaType();
	}

	public RawTimeInterval<T> createInterval(final Object point,
			final Object duration) {
		return createInterval(rawPointFactory.createPoint(point),
				naturalsFactory.generate(duration));
	}

	public RawTimeInterval<T> createInterval(final T point, final T duration) {
		return createInterval(rawPointFactory.createPoint(point), duration);
	}

	public RawTimeInterval<T> createInterval(final RawTimePoint<T> point,
			final T duration) {
		return new RawTimeInterval<T>(point, duration);
	}
	
	public RawTimeInterval<T> createRandomInterval() {
		final RawTimePoint<T> point = this.rawPointFactory.createRandomPoint();
		final T duration = this.naturalsFactory.getRandom();
		
		return createInterval(point, duration);
	}
}
