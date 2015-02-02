package net.meisen.dissertation.model.descriptors.mock;

import net.meisen.dissertation.impl.descriptors.DoubleDescriptor;
import net.meisen.dissertation.impl.descriptors.GeneralDescriptor;
import net.meisen.dissertation.impl.descriptors.IntegerDescriptor;
import net.meisen.dissertation.impl.idfactories.IntegerIdsFactory;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.descriptors.DescriptorPrimitiveDataType;

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
	public String getUniqueString() {
		return getValue().toString();
	}

	@Override
	public double getFactValue(final IDataRecord record) {
		return 0.0;
	}

	@Override
	public boolean isRecordInvariant() {
		return true;
	}

	@Override
	public boolean isValueInvariant() {
		return true;
	}
}
