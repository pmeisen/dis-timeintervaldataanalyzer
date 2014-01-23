package net.meisen.dissertation.data.impl.descriptors;

import net.meisen.dissertation.models.impl.data.Descriptor;
import net.meisen.dissertation.models.impl.data.DescriptorModel;

public class GeneralDescriptor<I extends Object> extends
		Descriptor<Object, GeneralDescriptor<I>, I> {
	private Object value;

	public GeneralDescriptor(final DescriptorModel model, final I id) {
		this(model, id, null);
	}

	public GeneralDescriptor(final DescriptorModel model, final I id,
			final Object value) {
		super(model, id);

		this.value = value;
	}

	@Override
	public Object getValue() {
		return value;
	}

	@Override
	public String toString() {
		return value == null ? null : value.toString();
	}

	@Override
	public GeneralDescriptor<I> clone() {
		return new GeneralDescriptor<I>(getModel(), getId(), value);
	}
}
