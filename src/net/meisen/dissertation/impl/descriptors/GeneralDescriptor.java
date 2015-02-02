package net.meisen.dissertation.impl.descriptors;

import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;

/**
 * A {@code Descriptor} defined by a general {@code Object} instances as value.
 * 
 * @author pmeisen
 * 
 * @param <I>
 *            the type of the identifier of the index used for the
 *            {@code Descriptor}
 */
public class GeneralDescriptor<I extends Object> extends
		Descriptor<Object, GeneralDescriptor<I>, I> {
	private Object value;

	/**
	 * A constructor which creates a {@code null}-{@code Object} descriptor.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 */
	public GeneralDescriptor(final DescriptorModel<I> model, final I id) {
		this(model, id, null);
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
	public GeneralDescriptor(final DescriptorModel<I> model, final I id,
			final Object value) {
		super(model, id);

		this.value = value;
	}

	@Override
	public Object getValue() {
		return value;
	}
	
	@Override
	public GeneralDescriptor<I> clone() {
		return new GeneralDescriptor<I>(getModel(), getId(), value);
	}

	@Override
	public String getUniqueString() {
		return value.toString();
	}
	
	@Override
	public double getFactValue(final IDataRecord record) {
		return 1.0;
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
