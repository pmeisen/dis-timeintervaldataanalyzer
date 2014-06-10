package net.meisen.dissertation.exceptions;

/**
 * Exception thrown whenever a {@code TidaServerListener} has a problem.
 * 
 * @author pmeisen
 * 
 */
public class TidaServerListenerException extends RuntimeException {
	private static final long serialVersionUID = -5106486457437603623L;

	/**
	 * Creates an exception which should been thrown whenever there is no other
	 * reason for the exception, i.e. the exception is the root.
	 * 
	 * @param message
	 *            the message of the exception
	 */
	public TidaServerListenerException(final String message) {
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
	public TidaServerListenerException(final String message, final Throwable t) {
		super(message, t);
	}
}
