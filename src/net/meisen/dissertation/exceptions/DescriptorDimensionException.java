package net.meisen.dissertation.exceptions;

import net.meisen.dissertation.model.dimensions.DescriptorDimension;
import net.meisen.dissertation.model.dimensions.DescriptorHierarchy;
import net.meisen.dissertation.model.dimensions.DescriptorLevel;
import net.meisen.dissertation.model.dimensions.DescriptorMember;

/**
 * Exception thrown whenever a problem with {@code DescriptorDimension},
 * {@code DescriptorHierarchy}, {@code DescriptorLevel} or
 * {@code DescriptorMember} occurs.
 * 
 * @author pmeisen
 * 
 * @see DescriptorDimension
 * @see DescriptorHierarchy
 * @see DescriptorLevel
 * @see DescriptorMember
 * 
 */
public class DescriptorDimensionException extends RuntimeException {
	private static final long serialVersionUID = 7268719248335330247L;

	/**
	 * Creates an exception which should been thrown whenever there is no other
	 * reason for the exception, i.e. the exception is the root.
	 * 
	 * @param message
	 *            the message of the exception
	 */
	public DescriptorDimensionException(final String message) {
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
	public DescriptorDimensionException(final String message, final Throwable t) {
		super(message, t);
	}
}
