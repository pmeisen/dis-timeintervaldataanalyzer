package net.meisen.dissertation.impl.descriptors;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import net.meisen.dissertation.model.data.Descriptor;
import net.meisen.dissertation.model.data.DescriptorModel;

/**
 * Simple test-{@code Descriptor} to handle {@code List} instances.
 * 
 * @author pmeisen
 * 
 * @param <I>
 *            the type of the indizes used
 */
public class ListDescriptor<I extends Object> extends
		Descriptor<String[], ListDescriptor<I>, I> {

	private String[] values;

	/**
	 * Constructor without any value assignment.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} of the {@code Descriptor}
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 */
	public ListDescriptor(final DescriptorModel<I> model, final I id) {
		this(model, id, (String) null);
	}

	/**
	 * Constructor with {@code String}-value assignment.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} of the {@code Descriptor}
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the value to be assigned
	 */
	public ListDescriptor(final DescriptorModel<I> model, final I id,
			final String value) {
		this(model, id, value == null ? new String[] {} : value.split(","));
	}

	/**
	 * Constructor with {@code Collection}-value assignment.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} of the {@code Descriptor}
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the value to be assigned
	 */
	public ListDescriptor(final DescriptorModel<I> model, final I id,
			final Collection<String> value) {
		this(model, id, value == null ? new String[] {} : value
				.toArray(new String[] {}));
	}

	/**
	 * Constructor with {@code String-Array}-value assignment.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} of the {@code Descriptor}
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the value to be assigned
	 */
	public ListDescriptor(final DescriptorModel<I> model, final I id,
			final String[] value) {
		super(model, id);

		this.values = value;
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
	public ListDescriptor<I> clone() {
		return null;
	}

	@Override
	public String toString() {
		return null;
	}
}
