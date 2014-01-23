package net.meisen.dissertation.models.impl.data.mock;

import net.meisen.dissertation.data.impl.descriptors.DoubleDescriptor;
import net.meisen.dissertation.data.impl.descriptors.GeneralDescriptor;
import net.meisen.dissertation.data.impl.descriptors.IntegerDescriptor;
import net.meisen.dissertation.data.impl.idfactories.IntegerIdsFactory;
import net.meisen.dissertation.models.impl.data.Descriptor;
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
		super(new DescriptorModel<Integer>("SomeId",
				getDescriptorClass(value.getClass()), new IntegerIdsFactory()),
				1);
		this.value = value;
	}

	@SuppressWarnings("rawtypes")
	private static Class<? extends Descriptor> getDescriptorClass(
			final Class<?> clazz) {
		if (clazz.equals(Integer.class)) {
			return IntegerDescriptor.class;
		} else if (clazz.equals(Double.class)) {
			return DoubleDescriptor.class;
		} else {
			return GeneralDescriptor.class;
		}
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
