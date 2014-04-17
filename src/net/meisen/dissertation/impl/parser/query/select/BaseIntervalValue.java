package net.meisen.dissertation.impl.parser.query.select;

/**
 * A base implementation for {@code IntervalValue} instances. An
 * {@code IntervalValue} is used within an interval as open or close value.
 * 
 * @author pmeisen
 * 
 * @param <T>
 *            the type of the value
 */
public abstract class BaseIntervalValue<T> {

	private T value;

	/**
	 * Constructor to specify the value of the {@code IntervalValue}.
	 * 
	 * @param value
	 *            the value of the {@code this}
	 */
	public BaseIntervalValue(final T value) {
		this.value = value;
	}

	/**
	 * Gets the type of the {@code IntervalValue}.
	 * 
	 * @return the type of the {@code this}
	 */
	public abstract Class<T> getType();

	/**
	 * Gets the value of the {@code IntervalValue}.
	 * 
	 * @return the value of the {@code this}
	 */
	public T getValue() {
		return value;
	}

	@Override
	public String toString() {
		return getValue() == null ? null : getValue().toString();
	}
}
