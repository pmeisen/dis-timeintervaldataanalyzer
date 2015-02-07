package net.meisen.dissertation.model.descriptors;

import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * A {@code DescriptorFactory} is used to create specific values of a
 * {@code Descriptor}. In general, the {@code DescriptorModel} can be seen as
 * the factory for {@code Descriptor} instances. Nevertheless, some things might
 * have to be handled specifically for some descriptors. Thus, a {@code
 * DescriptorFactory} can be used. The factory must be defined as <b>nested
 * static class</b> within the {@code Descriptor}. Additionally, it must provide
 * a default constructor (i.e. one without any parameters).
 * 
 * @author pmeisen
 * 
 */
public interface IDescriptorFactory {

	/**
	 * This method is used to format the specified value to a unique string.
	 * There might be several representatives for a value, which might be unique
	 * (e.g. 1.00, 1, 1.0000). Nevertheless, a descriptor must have a single
	 * unique representative, which is defined here, i.e. 1.00 => 1, 1.000 => 1,
	 * etc.
	 * 
	 * @param value
	 *            the string to be mapped to the unique representative
	 *            
	 * @return the unique string
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the value is not a valid descriptor value
	 */
	public String format(final String value) throws ForwardedRuntimeException;
}
