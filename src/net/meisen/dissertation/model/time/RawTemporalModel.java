package net.meisen.dissertation.model.time;

import net.meisen.dissertation.model.granularity.rawtime.IRawTimeGranularity;
import net.meisen.dissertation.model.naturals.INaturals;
import net.meisen.dissertation.model.naturals.INaturalsFactory;

public class RawTemporalModel<T extends INaturals<T>> implements IRawTemporalModel<T> {
	private final INaturalsFactory<T> factory;
	private final IRawTimeGranularity granularity;
	private final T start;
	private final T end;

	private Class<T> type = null;

	public RawTemporalModel(final INaturalsFactory<T> factory,
			final IRawTimeGranularity granularity) {
		this.factory = factory;
		this.granularity = granularity;
		this.start = factory.getZero();
		this.end = factory.getMax();

		// get the type for later usage
		@SuppressWarnings("unchecked")
		final Class<T> type = (Class<T>) start.getClass();
		this.type = type;
	}

	@Override
	public Class<T> getType() {
		return type;
	}

	@Override
	public IRawTimeGranularity getGranularity() {
		return granularity;
	}

	@Override
	public T getStart() {
		return start;
	}

	@Override
	public T getEnd() {
		return end;
	}

	@Override
	public T getNext(final T current) {

		// make sure the limit isn't reached
		if (current == null) {
			throw new NullPointerException("The current value cannot be null.");
		} else if (current.equals(getEnd())) {
			return null;
		}

		// increase the current value
		final T next = current.add(factory.getOne());

		// return the calculated next value
		return next;
	}
}
