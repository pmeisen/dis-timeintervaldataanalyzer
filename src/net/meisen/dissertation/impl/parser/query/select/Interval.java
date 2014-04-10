package net.meisen.dissertation.impl.parser.query.select;

/**
 * Specifies an interval, i.e. something like {@code [1, 5]},
 * {@code (1000, 5000]}, {@code [1, 5)}, or {@code (1, 8500)}.
 * 
 * @author pmeisen
 * 
 * @param <T>
 */
public class Interval<T> {

	private final IntervalValue<T> start;
	private final IntervalValue<T> end;

	private final IntervalType openType;
	private final IntervalType closeType;

	public Interval(final IntervalValue<T> start, final IntervalType openType,
			final IntervalValue<T> end, final IntervalType closeType) {

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

	public T getStart() {
		return this.start.getValue();
	}

	public T getEnd() {
		return this.end.getValue();
	}

	public IntervalType getOpenType() {
		return this.openType;
	}

	public IntervalType getCloseType() {
		return this.closeType;
	}

	public Class<T> getType() {
		return start.getType();
	}

	@Override
	public String toString() {
		return this.openType.toString(true) + getStart() + ", " + getEnd()
				+ this.closeType.toString(false);
	}
}
