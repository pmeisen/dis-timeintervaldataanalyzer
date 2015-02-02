package net.meisen.dissertation.model.indexes.datarecord.slices.mock;

import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;

/**
 * Just a simple variant descriptor.
 * 
 * @author pmeisen
 * 
 */
public class VariantDescriptor extends
		Descriptor<String, VariantDescriptor, Integer> {

	private String value;

	/**
	 * Default constructor...
	 * 
	 * @param model
	 *            the model
	 * @param id
	 *            the id
	 * @param value
	 *            the value
	 */
	public VariantDescriptor(final DescriptorModel<Integer> model,
			final Integer id, final String value) {
		super(model, id);

		this.value = value;
	}

	@Override
	public String getValue() {
		return value;
	}

	@Override
	public double getFactValue(final IDataRecord record) {
		return Math.random();
	}

	@Override
	public boolean isRecordInvariant() {
		return false;
	}
	
	@Override
	public boolean isValueInvariant() {
		return false;
	}

	@Override
	public String getUniqueString() {
		return value;
	}

	@Override
	public VariantDescriptor clone() {
		return new VariantDescriptor(getModel(), getId(), getValue());
	}
}
