package net.meisen.dissertation.impl.parser.query.select;

public abstract class IntervalValue<T> {

	private T value;

	public IntervalValue(final T value) {
		this.value = value;
	}

	public abstract Class<T> getType();

	public T getValue() {
		return value;
	}

	@Override
	public String toString() {
		return getValue() == null ? null : getValue().toString();
	}
}
