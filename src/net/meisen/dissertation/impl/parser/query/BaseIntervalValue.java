package net.meisen.dissertation.impl.parser.query;

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

	/**
	 * Create the {@code IntervalValue} for the specified val.
	 * 
	 * @param val
	 *            the value to create the {@code IntervalValue} for
	 * 
	 * @return the create {@code IntervalValue} representing the specified
	 *         {@code val}
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public static BaseIntervalValue<?> createVal(final Object val) {
		return new BaseIntervalValue(val) {

			@Override
			public Class getType() {
				return val.getClass();
			}
		};
	}
}
