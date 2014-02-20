package net.meisen.dissertation.exceptions;

/**
 * Exception thrown whenever a {@code BasePersistor} has a problem.
 * 
 * @author pmeisen
 * 
 */
public class PersistorException extends RuntimeException {
	private static final long serialVersionUID = 7268719248335330247L;

	/**
	 * Creates an exception which should been thrown whenever there is no other
	 * reason for the exception, i.e. the exception is the root.
	 * 
	 * @param message
	 *            the message of the exception
	 */
	public PersistorException(final String message) {
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
	public PersistorException(final String message, final Throwable t) {
		super(message, t);
	}
}
