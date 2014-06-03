package net.meisen.dissertation.impl.parser.query;


/**
 * A {@code Long} used as a value within an interval.
 * 
 * @author pmeisen
 */
public class LongIntervalValue extends BaseIntervalValue<Long> {

	/**
	 * Creates a {@code LongIntervalValue} for the specified {@code value}.
	 * 
	 * @param value
	 *            the {@code Long} of {@code this}
	 */
	public LongIntervalValue(final Long value) {
		super(value);
	}

	@Override
	public Class<Long> getType() {
		return Long.class;
	}
}
