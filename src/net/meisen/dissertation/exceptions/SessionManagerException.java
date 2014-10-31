package net.meisen.dissertation.exceptions;

/**
 * Exception thrown by the {@code SessionManager}
 * 
 * @author pmeisen
 * 
 */
public class SessionManagerException extends RuntimeException {
	private static final long serialVersionUID = 1949198649443706037L;

	/**
	 * Creates an exception which should been thrown whenever there is no other
	 * reason for the exception, i.e. the exception is the root.
	 * 
	 * @param message
	 *            the message of the exception
	 */
	public SessionManagerException(final String message) {
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
	public SessionManagerException(final String message, final Throwable t) {
		super(message, t);
	}
}
