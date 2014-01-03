package net.meisen.dissertation.data.impl.descriptors;

import net.meisen.dissertation.models.impl.data.DescriptorModel;
import net.meisen.dissertation.models.impl.data.DescriptorPrimitiveDataType;

public class IntegerDescriptor<I extends Object> extends
		DescriptorPrimitiveDataType<Integer, IntegerDescriptor<I>, I> {
	private int value;

	public IntegerDescriptor(final DescriptorModel model, final I id) {
		this(model, id, 0);
	}

	public IntegerDescriptor(final DescriptorModel model, final I id,
			final String value) {
		this(model, id, Integer.parseInt(value));
	}

	public IntegerDescriptor(final DescriptorModel model, final I id,
			final Integer value) {
		super(model, id);
		this.value = value;
	}
	
	public IntegerDescriptor(final DescriptorModel model, final I id,
			final Number value) {
		super(model, id);
		this.value = mapToDataType(value);
	}

	@Override
	public Integer getValue() {
		return value;
	}

	@Override
	public String toString() {
		return "" + value;
	}

	@Override
	public IntegerDescriptor<I> clone() {
		return new IntegerDescriptor<I>(getModel(), getId(), value);
	}

	@Override
	public Class<Integer> getPrimitiveDataType() {
		return Integer.class;
	}

	@Override
	public boolean valueEquals(final IntegerDescriptor<I> descriptor) {
		return descriptor.value == value;
	}
}
