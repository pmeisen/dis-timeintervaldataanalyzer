package net.meisen.dissertation.exceptions;

/**
 * Exception used whenever some general exception should be thrown.
 * 
 * @author pmeisen
 */
public class GeneralException extends RuntimeException {
	private static final long serialVersionUID = -2750597923776318821L;

	/**
	 * Creates an exception which should been thrown whenever there is no other
	 * reason for the exception, i.e. the exception is the root.
	 * 
	 * @param message
	 *            the message of the exception
	 */
	public GeneralException(final String message) {
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
	public GeneralException(final String message, final Throwable t) {
		super(message, t);
	}
}
