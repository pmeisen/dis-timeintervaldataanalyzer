package net.meisen.dissertation.exceptions;

import net.meisen.dissertation.model.data.metadata.IMetaDataCollection;

/**
 * Thrown whenever a problem with a {@code MetaDataCollection} occurs.
 * 
 * @author pmeisen
 * 
 * @see IMetaDataCollection
 * 
 */
public class MetaDataCollectionException extends RuntimeException {
	private static final long serialVersionUID = -2706323489200537049L;

	/**
	 * Creates an exception which should been thrown whenever there is no other
	 * reason for the exception, i.e. the exception is the root.
	 * 
	 * @param message
	 *            the message of the exception
	 */
	public MetaDataCollectionException(final String message) {
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
	public MetaDataCollectionException(final String message, final Throwable t) {
		super(message, t);
	}
}
