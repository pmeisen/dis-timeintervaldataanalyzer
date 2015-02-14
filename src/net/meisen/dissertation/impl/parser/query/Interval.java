package net.meisen.dissertation.impl.parser.query;

/**
 * Specifies an interval, i.e. something like {@code [1, 5]},
 * {@code (1000, 5000]}, {@code [1, 5)}, or {@code (1, 8500)}.
 * 
 * @author pmeisen
 * 
 * @param <T> the type of the value
 */
public class Interval<T> {

	private final BaseIntervalValue<T> start;
	private final BaseIntervalValue<T> end;

	private final IntervalType openType;
	private final IntervalType closeType;

	/**
	 * Creates an interval {@code [start, end]}.
	 * 
	 * @param start
	 *            the start value
	 * @param end
	 *            the end value
	 */
	public Interval(final BaseIntervalValue<T> start,
			final BaseIntervalValue<T> end) {
		this(start, IntervalType.INCLUDE, end, IntervalType.INCLUDE);
	}

	/**
	 * Creates an interval with the specified {@code start} and {@code end}
	 * value. The {@code IntervalType} instances define if the {@code start} and
	 * {@code end} is excluded or included, i.e. {@code [start, end]},
	 * {@code (start, end]}, {@code [start, end)} or (start, end)}.
	 * 
	 * @param start
	 *            the start value
	 * @param openType
	 *            the type of the start
	 * @param end
	 *            the end value
	 * @param closeType
	 *            the type of the end
	 */
	public Interval(final BaseIntervalValue<T> start,
			final IntervalType openType, final BaseIntervalValue<T> end,
			final IntervalType closeType) {

		if (start == null || end == null) {
			throw new NullPointerException("The start or end cannot be null.");
		} else if (!start.getType().equals(end.getType())) {
			throw new IllegalArgumentException(
					"The type of the start and end value must be equal ('"
							+ start.getType().getSimpleName() + "' != '"
							+ end.getType().getSimpleName() + "'.");
		}

		this.start = start;
		this.end = end;
		this.openType = openType;
		this.closeType = closeType;
	}

	/**
	 * Gets the value of the start.
	 * 
	 * @return the value of the start
	 */
	public T getStart() {
		return this.start.getValue();
	}

	/**
	 * Gets the value of the end.
	 * 
	 * @return the value of the end
	 */
	public T getEnd() {
		return this.end.getValue();
	}

	/**
	 * Gets the {@code IntervalType} of the start.
	 * 
	 * @return the {@code IntervalType} of the start
	 */
	public IntervalType getOpenType() {
		return this.openType;
	}

	/**
	 * Gets the {@code IntervalType} of the end.
	 * 
	 * @return the {@code IntervalType} of the end
	 */
	public IntervalType getCloseType() {
		return this.closeType;
	}

	/**
	 * Get the type of the values of the interval.
	 * 
	 * @return the type of the values of the interval
	 */
	public Class<T> getType() {
		return start.getType();
	}

	@Override
	public String toString() {
		return this.openType.toString(true) + getStart() + ", " + getEnd()
				+ this.closeType.toString(false);
	}
}
