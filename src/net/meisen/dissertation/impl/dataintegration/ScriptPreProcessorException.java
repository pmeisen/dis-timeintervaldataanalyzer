package net.meisen.dissertation.impl.dataintegration;

/**
 * General exceptions thrown within a {@code ScriptPreProcessor}.
 * 
 * @author pmeisen
 * 
 * @see ScriptPreProcessor
 * 
 */
public class ScriptPreProcessorException extends RuntimeException {
	private static final long serialVersionUID = -5712132524861270648L;

	/**
	 * Creates an exception which should been thrown whenever there is no other
	 * reason for the exception, i.e. the exception is the root.
	 * 
	 * @param message
	 *            the message of the exception
	 */
	public ScriptPreProcessorException(final String message) {
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
	public ScriptPreProcessorException(final String message, final Throwable t) {
		super(message, t);
	}
}
