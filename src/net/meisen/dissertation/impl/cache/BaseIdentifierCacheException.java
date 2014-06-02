package net.meisen.dissertation.impl.cache;

/**
 * Default exception used by the {@code BaseIdentifierCache}.
 * 
 * @author pmeisen
 * 
 */
public class BaseIdentifierCacheException extends RuntimeException {
	private static final long serialVersionUID = 8874856053169639071L;

	/**
	 * Creates an exception which should been thrown whenever there is no other
	 * reason for the exception, i.e. the exception is the root.
	 * 
	 * @param message
	 *            the message of the exception
	 */
	public BaseIdentifierCacheException(final String message) {
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
	public BaseIdentifierCacheException(final String message, final Throwable t) {
		super(message, t);
	}
}
