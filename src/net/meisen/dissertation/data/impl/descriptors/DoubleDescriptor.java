package net.meisen.dissertation.data.impl.descriptors;

import net.meisen.dissertation.models.impl.data.DescriptorModel;
import net.meisen.dissertation.models.impl.data.DescriptorPrimitiveDataType;

public class DoubleDescriptor<I extends Object> extends
		DescriptorPrimitiveDataType<Double, DoubleDescriptor<I>, I> {
	private double value;

	public DoubleDescriptor(final DescriptorModel model, final I id) {
		this(model, id, 0.0);
	}

	public DoubleDescriptor(final DescriptorModel model, final I id,
			final Integer value) {
		this(model, id, value.doubleValue());
	}

	public DoubleDescriptor(final DescriptorModel model, final I id,
			final Long value) {
		this(model, id, value.doubleValue());
	}

	public DoubleDescriptor(final DescriptorModel model, final I id,
			final String value) {
		this(model, id, Double.parseDouble(value));
	}

	public DoubleDescriptor(final DescriptorModel model, final I id,
			final Double value) {
		super(model, id);
		this.value = value;
	}

	@Override
	public Double getValue() {
		return value;
	}

	@Override
	public String toString() {
		return "" + value;
	}

	@Override
	public DoubleDescriptor<I> clone() {
		return new DoubleDescriptor<I>(getModel(), getId(), value);
	}

	@Override
	public Class<Double> getPrimitiveDataType() {
		return Double.class;
	}

	@Override
	public boolean valueEquals(final DoubleDescriptor<I> descriptor) {
		return descriptor.value == value;
	}
}
