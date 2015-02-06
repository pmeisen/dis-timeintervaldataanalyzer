package net.meisen.dissertation.exceptions;

/**
 * Exception thrown when an error with the {@code LoaderServlet} occurred.
 * 
 * @author pmeisen
 * 
 */
public class LoaderServletException extends ServletException {
	private static final long serialVersionUID = 5100510057688264976L;

	/**
	 * Creates an exception which should been thrown whenever there is no other
	 * reason for the exception, i.e. the exception is the root.
	 * 
	 * @param message
	 *            the message of the exception
	 */
	public LoaderServletException(final String message) {
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
	public LoaderServletException(final String message, final Throwable t) {
		super(message, t);
	}
}
