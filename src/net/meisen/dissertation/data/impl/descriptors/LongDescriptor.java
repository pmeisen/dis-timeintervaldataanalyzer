package net.meisen.dissertation.data.impl.descriptors;

import net.meisen.dissertation.models.impl.data.DescriptorModel;
import net.meisen.dissertation.models.impl.data.DescriptorPrimitiveDataType;

public class LongDescriptor<I extends Object> extends
		DescriptorPrimitiveDataType<Long, LongDescriptor<I>, I> {
	private long value;

	public LongDescriptor(final DescriptorModel model, final I id) {
		this(model, id, 0l);
	}

	public LongDescriptor(final DescriptorModel model, final I id,
			final Integer value) {
		this(model, id, value.longValue());
	}

	public LongDescriptor(final DescriptorModel model, final I id,
			final String value) {
		this(model, id, Long.parseLong(value));
	}

	public LongDescriptor(final DescriptorModel model, final I id,
			final Long value) {
		super(model, id);
		this.value = value;
	}

	@Override
	public Long getValue() {
		return value;
	}

	@Override
	public String toString() {
		return "" + value;
	}

	@Override
	public LongDescriptor<I> clone() {
		return new LongDescriptor<I>(getModel(), getId(), value);
	}

	@Override
	public Class<Long> getPrimitiveDataType() {
		return Long.class;
	}

	@Override
	public boolean valueEquals(LongDescriptor<I> descriptor) {
		return descriptor.value == value;
	}
}
