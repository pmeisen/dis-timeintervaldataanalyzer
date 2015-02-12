package net.meisen.dissertation.model.measures;

import net.meisen.dissertation.exceptions.GeneralException;
import net.meisen.dissertation.model.util.IDoubleIterator;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Objects;

/**
 * Base implementation of an {@code AggregationFunction}.
 * 
 * @author pmeisen
 * 
 */
public abstract class BaseAggregationFunction implements IAggregationFunction {
	private Class<? extends IAggregationFunction> definedType = null;

	@Override
	public void setDefinedType(final Class<? extends IAggregationFunction> type)
			throws ForwardedRuntimeException {
		if (type == null || !type.isAssignableFrom(this.getClass())) {
			throw new ForwardedRuntimeException(GeneralException.class, 1001,
					getName(), type.getSimpleName());
		}

		this.definedType = type;
	}

	@Override
	public Class<? extends IAggregationFunction> getDefinedType() {
		return definedType;
	}

	@Override
	public String toString() {
		return getName().toUpperCase();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == this) {
			return true;
		} else if (obj == null) {
			return false;
		} else if (obj.getClass().equals(this.getClass())) {
			return Objects.equals(getDefinedType(),
					((IAggregationFunction) obj).getDefinedType());
		} else {
			return false;
		}
	}

	/**
	 * Helper method to calculate the sum of the specified {@code facts}. The
	 * {@link #getDefaultValue()} is returned, if no facts are defined.
	 * Generally, {@code Double.NaN} facts are ignored when summing up and
	 * {@link #getNaNValue()} is returned if the amount of non-NaN facts is 0.
	 * 
	 * @param facts
	 *            the facts to calculate the sum for
	 * 
	 * @return the sum of the facts
	 */
	protected double sum(final IDoubleHolder facts) {

		// make sure we have values
		if (facts == null || facts.amount() == 0) {
			return getDefaultValue();
		} else if (facts.amountOfNonNaN() == 0) {
			return getNaNValue();
		} else {

			// otherwise get the sum and calculate the average
			double sum = 0.0;
			final IDoubleIterator it = facts.iterator(true);
			while (it.hasNext()) {
				sum += it.next();
			}

			return sum;
		}
	}

	/**
	 * Finds the first non {@code Double#NaN} in the iterator.
	 * 
	 * @param it
	 *            the iterator to search in
	 * 
	 * @return the first none {@code Double#NaN} value, {@link #getNaNValue()}
	 *         if only NaN-values are in there, or the default value if there
	 *         was no value within the iterator
	 */
	public double findFirstNotNaN(final IDoubleIterator it) {
		if (it.hasNext()) {
			/*
			 * According to the definition of the DoubleIterator, a NaN value
			 * has to be last in the line, independent of the sorting order.
			 */
			final double val = it.next();
			if (Double.isNaN(val)) {
				return getNaNValue();
			} else {
				return val;
			}
		} else {
			return getDefaultValue();
		}
	}

	@Override
	public IAggregationFunction create() throws ForwardedRuntimeException {
		try {
			return this.getClass().newInstance();
		} catch (final Exception e) {
			throw new ForwardedRuntimeException(GeneralException.class, 1000,
					getName());
		}
	}

	@Override
	public double getDefaultValue() {
		return Double.NaN;
	}

	@Override
	public double getNaNValue() {
		return Double.NaN;
	}
}
