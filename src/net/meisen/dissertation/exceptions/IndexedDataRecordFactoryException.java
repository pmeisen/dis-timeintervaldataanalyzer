package net.meisen.dissertation.exceptions;

import net.meisen.dissertation.impl.indexes.IndexFactory;

/**
 * Exception thrown whenever a {@code IndexedDataRecordFactory} has a problem.
 * 
 * @see IndexFactory
 * 
 * @author pmeisen
 * 
 */
public class IndexedDataRecordFactoryException extends RuntimeException {
	private static final long serialVersionUID = -3669111551903950730L;

	/**
	 * Creates an exception which should been thrown whenever there is no other
	 * reason for the exception, i.e. the exception is the root.
	 * 
	 * @param message
	 *            the message of the exception
	 */
	public IndexedDataRecordFactoryException(final String message) {
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
	public IndexedDataRecordFactoryException(final String message,
			final Throwable t) {
		super(message, t);
	}
}
