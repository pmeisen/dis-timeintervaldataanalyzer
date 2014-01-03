package net.meisen.dissertation.models.impl.data.mock;

import net.meisen.dissertation.models.impl.data.DescriptorModel;
import net.meisen.dissertation.models.impl.data.DescriptorPrimitiveDataType;

/**
 * Mock to test the implementation of a {@code DescriptorPrimitiveDataType}.
 * 
 * @author pmeisen
 * 
 * @param <D>
 *            the primitive data type
 */
public class MockDescriptorPrimitiveDataType<D>
		extends
		DescriptorPrimitiveDataType<D, MockDescriptorPrimitiveDataType<D>, Integer> {

	private D value;

	/**
	 * Default constructor
	 * 
	 * @param value
	 *            the value cannot be {@code null}
	 */
	public MockDescriptorPrimitiveDataType(final D value) {
		super(new DescriptorModel("SomeId", value.getClass()), 1);
		this.value = value;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Class<D> getPrimitiveDataType() {
		return (Class<D>) value.getClass();
	}

	@Override
	public boolean valueEquals(
			final MockDescriptorPrimitiveDataType<D> descriptor) {
		return descriptor.value.equals(value);
	}

	@Override
	public D getValue() {
		return value;
	}

	@Override
	public MockDescriptorPrimitiveDataType<D> clone() {
		return this;
	}

	@Override
	public String toString() {
		return "" + getValue();
	}
}
