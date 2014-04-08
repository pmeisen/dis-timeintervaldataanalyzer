package net.meisen.dissertation.exceptions;

/**
 * Exception thrown when an error occurred parsing an {@code Query}.
 * 
 * @author pmeisen
 * 
 */
public class QueryParsingException extends RuntimeException {
	private static final long serialVersionUID = -5520135317115592210L;

	/**
	 * Creates an exception which should been thrown whenever there is no other
	 * reason for the exception, i.e. the exception is the root.
	 * 
	 * @param message
	 *            the message of the exception
	 */
	public QueryParsingException(final String message) {
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
	public QueryParsingException(final String message, final Throwable t) {
		super(message, t);
	}
}
