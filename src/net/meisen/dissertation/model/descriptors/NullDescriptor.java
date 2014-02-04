package net.meisen.dissertation.model.descriptors;

public class NullDescriptor<I extends Object> extends
		Descriptor<Object, NullDescriptor<I>, I> {

	private final String name;

	public NullDescriptor(final DescriptorModel<I> model, final I id) {
		this("", model, id);
	}

	public NullDescriptor(final String name, final DescriptorModel<I> model,
			final I id) {
		super(model, id);
		this.name = name;
	}

	@Override
	public Object getValue() {
		return null;
	}

	@Override
	public NullDescriptor<I> clone() {
		return this;
	}

	@Override
	public String toString() {
		return name;
	}

}
