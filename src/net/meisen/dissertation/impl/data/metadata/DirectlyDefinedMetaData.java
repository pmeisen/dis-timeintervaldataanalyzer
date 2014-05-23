package net.meisen.dissertation.impl.data.metadata;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import net.meisen.dissertation.model.data.metadata.IMetaData;
import net.meisen.general.genmisc.types.Objects;

/**
 * {@code DirectlyDefinedMetaData} is used to define {@code MetaData} directly,
 * i.e. by values.
 * 
 * @author pmeisen
 * 
 */
public class DirectlyDefinedMetaData implements IMetaData {
	private final List<Object> values;
	private final String descriptorModelId;

	/**
	 * The constructor creates an empty list of meta-data for the specified
	 * {@code descriptorModelId}.
	 * 
	 * @param descriptorModelId
	 *            the identifier of the {@code DescriptorModel} the values
	 *            belong to
	 */
	public DirectlyDefinedMetaData(final String descriptorModelId) {
		this.values = new ArrayList<Object>();
		this.descriptorModelId = descriptorModelId;
	}

	/**
	 * The constructor creates meta-data for the specified
	 * {@code descriptorModelId} with the specified {@code values}.
	 * 
	 * @param descriptorModelId
	 *            the identifier of the {@code DescriptorModel} the values
	 *            belong to
	 * @param values
	 *            the values to be added
	 */
	public DirectlyDefinedMetaData(final String descriptorModelId,
			final Collection<Object> values) {
		this(descriptorModelId);

		addValues(values);
	}

	/**
	 * The constructor creates meta-data for the specified
	 * {@code descriptorModelId} with the specified {@code values}.
	 * 
	 * @param descriptorModelId
	 *            the identifier of the {@code DescriptorModel} the values
	 *            belong to
	 * @param values
	 *            the values to be added
	 */
	public DirectlyDefinedMetaData(final String descriptorModelId,
			final Object... values) {
		this(descriptorModelId);

		addValues(values);
	}

	/**
	 * Adds the specified {@code values} to {@code this}.
	 * 
	 * @param values
	 *            the values to be added
	 */
	public void addValues(final Collection<Object> values) {
		addValues(values == null ? null : values.toArray());
	}

	/**
	 * Adds the specified {@code values} to {@code this}.
	 * 
	 * @param values
	 *            the values to be added
	 */
	public void addValues(final Object... values) {
		if (values == null) {
			return;
		}

		for (final Object value : values) {
			addValue(value);
		}
	}

	/**
	 * Adds the specified {@code value} to {@code this}.
	 * 
	 * @param value
	 *            the value to be added
	 */
	public void addValue(final Object value) {
		this.values.add(value);
	}

	/**
	 * Sets the specified {@code values} for {@code this}. Setting means (in
	 * contrast to adding) that all current values are removed.
	 * 
	 * @param values
	 *            the values to be set
	 */
	public void setValues(final Collection<Object> values) {
		setValues(values == null ? null : values.toArray());
	}

	/**
	 * Sets the specified {@code values} for {@code this}. Setting means (in
	 * contrast to adding) that all current values are removed.
	 * 
	 * @param values
	 *            the values to be set
	 */
	public void setValues(final Object... values) {
		this.values.clear();
		addValues(values);
	}

	@Override
	public Collection<Object> getValues() {
		return values;
	}

	@Override
	public String getDescriptorModelId() {
		return descriptorModelId;
	}

	@Override
	public String toString() {
		return getDescriptorModelId() + ": " + getValues();
	}

	@Override
	public int size() {
		return values.size();
	}
	
	@Override
	public boolean equals(final Object obj) {
		if (obj == this) {
			return true;
		} else if (obj instanceof DirectlyDefinedMetaData) {
			final DirectlyDefinedMetaData ddmd = (DirectlyDefinedMetaData) obj;
			return Objects.equals(ddmd.descriptorModelId, descriptorModelId)
					&& Objects.equals(ddmd.values, values);
		} else {
			return false;
		}
	}
}
