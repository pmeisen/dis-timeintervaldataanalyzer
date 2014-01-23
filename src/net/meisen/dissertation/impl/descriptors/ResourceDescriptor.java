package net.meisen.dissertation.impl.descriptors;

import net.meisen.dissertation.model.data.Descriptor;
import net.meisen.dissertation.model.data.DescriptorModel;

/**
 * A {@code ResourceDescriptor} is something which is consumed by an interval,
 * i.e. it identifies that this interval blocks the resource.
 * 
 * @author pmeisen
 * 
 * @param <I>
 *            the type of the identifier used by the resource
 */
public class ResourceDescriptor<I extends Object> extends
		Descriptor<String, ResourceDescriptor<I>, I> {
	private final String value;

	/**
	 * Constructor used to create a {@code ResourceDescriptor}.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} which defines this
	 *            {@code ResourceDescriptor}
	 * @param id
	 *            the identifier of the {@code ResourceDescriptor}
	 * @param value
	 *            the value of the {@code ResourceDescriptor}
	 */
	public ResourceDescriptor(final DescriptorModel<I> model, final I id,
			final String value) {
		super(model, id);

		this.value = value;
	}

	@Override
	public String getValue() {
		return value;
	}

	@Override
	public ResourceDescriptor<I> clone() {
		final ResourceDescriptor<I> clone = new ResourceDescriptor<I>(
				getModel(), getId(), getValue());

		return clone;
	}

	@Override
	public String toString() {
		return value;
	}
}
