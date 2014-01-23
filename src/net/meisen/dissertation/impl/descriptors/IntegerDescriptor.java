package net.meisen.dissertation.impl.descriptors;

import net.meisen.dissertation.model.data.DescriptorModel;
import net.meisen.dissertation.model.data.DescriptorPrimitiveDataType;

/**
 * A {@code Descriptor} defined by a {@code Integer} value.
 * 
 * @author pmeisen
 * 
 * @param <I>
 *            the type of the identifier of the index used for the
 *            {@code Descriptor}
 */
public class IntegerDescriptor<I extends Object> extends
		DescriptorPrimitiveDataType<Integer, IntegerDescriptor<I>, I> {
	private int value;

	/**
	 * A constructor which creates a {@code 0}-{@code Integer} descriptor.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 */
	public IntegerDescriptor(final DescriptorModel<I> model, final I id) {
		this(model, id, 0);
	}

	/**
	 * A constructor which creates a {@code value}-{@code Integer} descriptor.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the string which specifies the integer-value of the descriptor
	 */
	public IntegerDescriptor(final DescriptorModel<I> model, final I id,
			final String value) {
		this(model, id, Integer.parseInt(value));
	}

	/**
	 * A constructor which creates a {@code value}-{@code Integer} descriptor.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the integer which specifies the integer-value of the
	 *            descriptor
	 */
	public IntegerDescriptor(final DescriptorModel<I> model, final I id,
			final Integer value) {
		super(model, id);
		this.value = value;
	}

	/**
	 * A constructor which creates a {@code value}-{@code Integer} descriptor.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the number which specifies the integer-value of the descriptor
	 */
	public IntegerDescriptor(final DescriptorModel<I> model, final I id,
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
