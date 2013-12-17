package net.meisen.dissertation.config.xslt.mock;

import java.util.Arrays;
import java.util.List;

import net.meisen.dissertation.models.impl.data.Descriptor;
import net.meisen.dissertation.models.impl.data.DescriptorModel;

/**
 * Simple test-{@code Descriptor} to handle {@code List} instances.
 * 
 * @author pmeisen
 * 
 * @param <I>
 *            the type of the indizes used
 */
public class MyOwnTestDescriptor<I extends Object> extends
		Descriptor<String[], MyOwnTestDescriptor<I>, I> {

	private String[] values;

	/**
	 * Constructor without any value assignment.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} of the {@code Descriptor}
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 */
	public MyOwnTestDescriptor(final DescriptorModel model, final I id) {
		this(model, id, null);
	}

	/**
	 * Constructor with value assignment.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} of the {@code Descriptor}
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the value to be assigned
	 */
	public MyOwnTestDescriptor(final DescriptorModel model, final I id,
			final String value) {
		super(model, id);

		if (value == null) {
			values = new String[] {};
		} else {
			values = value.split(",");
		}
	}

	/**
	 * Gets the assigned {@code List} of values.
	 * 
	 * @return the assigned {@code List} of values
	 */
	public List<String> getValueList() {
		return Arrays.asList(values);
	}

	/**
	 * Get the values as array.
	 * 
	 * @return the values as array
	 */
	public String[] getValue() {
		return values;
	}

	@Override
	public MyOwnTestDescriptor<I> clone() {
		return null;
	}

	@Override
	public String toString() {
		return null;
	}
}
