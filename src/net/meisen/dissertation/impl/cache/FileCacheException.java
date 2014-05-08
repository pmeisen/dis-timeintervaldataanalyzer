package net.meisen.dissertation.impl.cache;

import net.meisen.dissertation.model.dataretriever.BaseDataRetriever;

/**
 * General exceptions thrown within a {@code FileCache}.
 * 
 * @author pmeisen
 * 
 * @see BaseDataRetriever
 * 
 */
public class FileCacheException extends RuntimeException {
	private static final long serialVersionUID = 851688924231263967L;

	/**
	 * Creates an exception which should been thrown whenever there is no other
	 * reason for the exception, i.e. the exception is the root.
	 * 
	 * @param message
	 *            the message of the exception
	 */
	public FileCacheException(final String message) {
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
	public FileCacheException(final String message, final Throwable t) {
		super(message, t);
	}
}
