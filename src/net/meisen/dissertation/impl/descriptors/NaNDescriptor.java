package net.meisen.dissertation.impl.descriptors;

import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.descriptors.DescriptorModel;

/**
 * A String-based record generating {@code Double.NaN} values if "NAN" is used
 * as name.
 * 
 * @author pmeisen
 * 
 * @param <I>
 *            the type of the identifier
 */
public class NaNDescriptor<I extends Object> extends GeneralDescriptor<I> {

	/**
	 * A constructor which creates a {@code null}-{@code Object} descriptor.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 */
	public NaNDescriptor(final DescriptorModel<I> model, final I id) {
		super(model, id);
	}

	/**
	 * A constructor which creates a {@code value}-{@code Object} descriptor.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the object which specifies the value of the descriptor
	 */
	public NaNDescriptor(final DescriptorModel<I> model, final I id,
			final String value) {
		super(model, id, value);
	}

	@Override
	public double getFactValue(final IDataRecord record) {
		return "NAN".equalsIgnoreCase(getValue().toString()) ? Double.NaN : 1.0;
	}

	@Override
	public boolean isValueInvariant() {
		return false;
	}
}