package net.meisen.dissertation.exceptions;

/**
 * Exception thrown whenever a permission is needed but not granted to the
 * current user.
 * 
 * @author pmeisen
 * 
 */
public class PermissionException extends AuthException {
	private static final long serialVersionUID = 8277847146171834320L;

	/**
	 * Creates an exception which should been thrown whenever there is no other
	 * reason for the exception, i.e. the exception is the root.
	 * 
	 * @param message
	 *            the message of the exception
	 */
	public PermissionException(final String message) {
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
	public PermissionException(final String message, final Throwable t) {
		super(message, t);
	}
}
