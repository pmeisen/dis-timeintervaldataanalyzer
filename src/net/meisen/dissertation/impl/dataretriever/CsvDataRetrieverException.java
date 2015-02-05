package net.meisen.dissertation.impl.dataretriever;

import net.meisen.dissertation.model.dataretriever.BaseDataRetriever;

/**
 * General exceptions thrown within a {@code CsvDataRetrieverException}.
 * 
 * @author pmeisen
 * 
 * @see BaseDataRetriever
 * @see CsvDataRetrieverException
 * 
 */
public class CsvDataRetrieverException extends RuntimeException {
	private static final long serialVersionUID = -7499900599875169852L;

	/**
	 * Creates an exception which should been thrown whenever there is no other
	 * reason for the exception, i.e. the exception is the root.
	 * 
	 * @param message
	 *            the message of the exception
	 */
	public CsvDataRetrieverException(final String message) {
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
	public CsvDataRetrieverException(final String message, final Throwable t) {
		super(message, t);
	}
}
