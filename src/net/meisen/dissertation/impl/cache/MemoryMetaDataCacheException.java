package net.meisen.dissertation.impl.cache;

/**
 * Exception thrown when a problem with the {@code MemoryMetaDataCache}
 * occurres.
 * 
 * @author pmeisen
 * 
 */
public class MemoryMetaDataCacheException extends RuntimeException {
	private static final long serialVersionUID = -2706323489200537049L;

	/**
	 * Creates an exception which should been thrown whenever there is no other
	 * reason for the exception, i.e. the exception is the root.
	 * 
	 * @param message
	 *            the message of the exception
	 */
	public MemoryMetaDataCacheException(final String message) {
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
	public MemoryMetaDataCacheException(final String message, final Throwable t) {
		super(message, t);
	}
}
