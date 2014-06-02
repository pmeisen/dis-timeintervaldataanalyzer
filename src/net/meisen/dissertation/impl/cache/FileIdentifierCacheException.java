package net.meisen.dissertation.impl.cache;

/**
 * Exception thrown whenever a problem with the {@code FileIdentifierCache}
 * occurred.
 * 
 * @author pmeisen
 * 
 */
public class FileIdentifierCacheException extends RuntimeException {
	private static final long serialVersionUID = -7700168531829125656L;

	/**
	 * Creates an exception which should been thrown whenever there is no other
	 * reason for the exception, i.e. the exception is the root.
	 * 
	 * @param message
	 *            the message of the exception
	 */
	public FileIdentifierCacheException(final String message) {
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
	public FileIdentifierCacheException(final String message, final Throwable t) {
		super(message, t);
	}
}
