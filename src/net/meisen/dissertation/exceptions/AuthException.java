package net.meisen.dissertation.exceptions;

/**
 * Exception used whenever a user was unable to authenticate (e.g. because of an
 * invalid password or a locked account).
 * 
 * @author pmeisen
 */
public class AuthException extends RuntimeException {
	private static final long serialVersionUID = -6430882835793129389L;

	/**
	 * Creates an exception which should been thrown whenever there is no other
	 * reason for the exception, i.e. the exception is the root.
	 * 
	 * @param message
	 *            the message of the exception
	 */
	public AuthException(final String message) {
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
	public AuthException(final String message, final Throwable t) {
		super(message, t);
	}
}
