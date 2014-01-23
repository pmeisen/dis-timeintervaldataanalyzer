package net.meisen.dissertation.impl.descriptors;

import net.meisen.dissertation.model.data.DescriptorModel;
import net.meisen.dissertation.model.data.DescriptorPrimitiveDataType;

/**
 * A {@code Descriptor} defined by a {@code Double} value.
 * 
 * @author pmeisen
 * 
 * @param <I>
 *            the type of the identifier of the index used for the
 *            {@code Descriptor}
 */
public class DoubleDescriptor<I extends Object> extends
		DescriptorPrimitiveDataType<Double, DoubleDescriptor<I>, I> {
	private double value;

	/**
	 * A constructor which creates a {@code 0.0}-{@code Double} descriptor.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 */
	public DoubleDescriptor(final DescriptorModel<I> model, final I id) {
		this(model, id, 0.0);
	}

	/**
	 * A constructor which creates a {@code value}-{@code Double} descriptor.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the integer which specifies the double-value of the descriptor
	 */
	public DoubleDescriptor(final DescriptorModel<I> model, final I id,
			final Integer value) {
		this(model, id, value.doubleValue());
	}

	/**
	 * A constructor which creates a {@code value}-{@code Double} descriptor.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the long which specifies the double-value of the descriptor
	 */
	public DoubleDescriptor(final DescriptorModel<I> model, final I id,
			final Long value) {
		this(model, id, value.doubleValue());
	}

	/**
	 * A constructor which creates a {@code value}-{@code Double} descriptor.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the string which specifies the double-value of the descriptor
	 */
	public DoubleDescriptor(final DescriptorModel<I> model, final I id,
			final String value) {
		this(model, id, Double.parseDouble(value));
	}

	/**
	 * A constructor which creates a {@code value}-{@code Double} descriptor.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the double which specifies the double-value of the descriptor
	 */
	public DoubleDescriptor(final DescriptorModel<I> model, final I id,
			final Double value) {
		super(model, id);
		this.value = value;
	}

	/**
	 * A constructor which creates a {@code value}-{@code Double} descriptor.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the number which specifies the double-value of the descriptor
	 */
	public DoubleDescriptor(final DescriptorModel<I> model, final I id,
			final Number value) {
		super(model, id);
		this.value = mapToDataType(value);
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
