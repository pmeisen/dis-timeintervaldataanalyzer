package net.meisen.dissertation.impl.parser.query;

/**
 * Defines the different types of intervals possible, i.e. {@code [..., ...)},
 * {@code (..., ...)}, {@code [..., ...]}, {@code (..., ...]}.
 * 
 * @author pmeisen
 * 
 */
public enum IntervalType {
	/**
	 * Defines the type to be included, i.e. a {@code [} or {@code ]} is used.
	 */
	INCLUDE("[", "]"),
	/**
	 * Defines the type to be included, i.e. a {@code (} or {@code )} is used.
	 */
	EXCLUDE("(", ")");

	private final String open;
	private final String close;

	private IntervalType(final String open, final String close) {
		this.open = open;
		this.close = close;
	}

	@Override
	public String toString() {
		return open;
	}

	/**
	 * Method which uses the open or close symbol for printing.
	 * 
	 * @param asOpen
	 *            {@code true} if the interval is open, otherwise {@code false}
	 * 
	 * @return the string representation of the interval type
	 */
	public String toString(final boolean asOpen) {
		return asOpen ? open : close;
	}

	/**
	 * Determines if the type is an inclusive or exclusive type.
	 * 
	 * @return {@code true} if the type is inclusive, otherwise {@code false}
	 */
	public boolean isInclusive() {
		return INCLUDE.equals(this);
	}
}
