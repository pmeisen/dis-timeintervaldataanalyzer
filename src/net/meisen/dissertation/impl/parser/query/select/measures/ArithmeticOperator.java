package net.meisen.dissertation.impl.parser.query.select.measures;

/**
 * A {@code MathOperator} is used to define mathematical expressions. The
 * specified {@code MathOperator}.
 * 
 * @author pmeisen
 * 
 */
public enum ArithmeticOperator {
	/**
	 * A mathematical {@code +}.
	 */
	ADD,
	/**
	 * A mathematical {@code -}.
	 */
	MINUS,
	/**
	 * A mathematical {@code x}.
	 */
	MULTIPLY,
	/**
	 * A mathematical {@code /}.
	 */
	DIVIDE;

	/**
	 * Applies the arithmetic operator to the specified parameters.
	 * 
	 * @param res1
	 *            the first parameter
	 * @param res2
	 *            the second parameter
	 * 
	 * @return the result
	 */
	public double apply(final double res1, final double res2) {
		if (Double.isNaN(res1) || Double.isNaN(res2)) {
			return Double.NaN;
		}

		switch (this) {
		case ADD:
			return res1 + res2;
		case MINUS:
			return res1 - res2;
		case MULTIPLY:
			return res1 * res2;
		case DIVIDE:
			if (res2 == 0.0) {
				return Double.NaN;
			} else {
				return res1 / res2;
			}
		default:
			throw new IllegalStateException(
					"A ArithmeticOperator is undefined, please specifiy '"
							+ this + "'.");
		}
	}
}
