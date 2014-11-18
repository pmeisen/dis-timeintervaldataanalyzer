package net.meisen.dissertation.exceptions;

/**
 * Exception thrown whenever a problem with {@code DimensionModel} occurs.
 * 
 * @author pmeisen
 * 
 */
public class DimensionModelException extends RuntimeException {
	private static final long serialVersionUID = 2955020043835922843L;

	/**
	 * Creates an exception which should been thrown whenever there is no other
	 * reason for the exception, i.e. the exception is the root.
	 * 
	 * @param message
	 *            the message of the exception
	 */
	public DimensionModelException(final String message) {
		super(message);
	}

	/**
	 * Creates an exception which should been thrown whenever another
	 * <code>Throwable</code> is the reason for this.
	 * 
	 * @param message
	 *            the message of the exception
	 * @param t
	 *            the reason for the exception
	 */
	public DimensionModelException(final String message, final Throwable t) {
		super(message, t);
	}
}
