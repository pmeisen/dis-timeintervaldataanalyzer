package net.meisen.dissertation.exceptions;

/**
 * Exception thrown when an error occurred evaluating an {@code Query}.
 * 
 * @author pmeisen
 * 
 */
public class QueryEvaluationException extends RuntimeException {
	private static final long serialVersionUID = -8002716122648672342L;

	/**
	 * Creates an exception which should been thrown whenever there is no other
	 * reason for the exception, i.e. the exception is the root.
	 * 
	 * @param message
	 *            the message of the exception
	 */
	public QueryEvaluationException(final String message) {
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
	public QueryEvaluationException(final String message, final Throwable t) {
		super(message, t);
	}
}