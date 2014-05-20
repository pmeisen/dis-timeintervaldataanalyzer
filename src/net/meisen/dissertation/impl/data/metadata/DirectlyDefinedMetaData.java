package net.meisen.dissertation.impl.data.metadata;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import net.meisen.dissertation.model.data.metadata.IMetaData;

public class DirectlyDefinedMetaData implements IMetaData {
	private final List<Object> values;
	private final String descriptorModelId;

	public DirectlyDefinedMetaData(final String descriptorModelId) {
		this.values = new ArrayList<Object>();
		this.descriptorModelId = descriptorModelId;
	}

	public DirectlyDefinedMetaData(final String descriptorModelId,
			final Collection<Object> values) {
		this(descriptorModelId);

		addValues(values);
	}

	public DirectlyDefinedMetaData(final String descriptorModelId,
			final Object... values) {
		this(descriptorModelId);

		addValues(values);
	}

	public void addValues(final Collection<Object> values) {
		addValues(values == null ? null : values.toArray());
	}

	public void addValues(final Object... values) {
		if (values == null) {
			return;
		}

		for (final Object value : values) {
			addValue(value);
		}
	}

	public void addValue(final Object value) {
		this.values.add(value);
	}

	public void setValues(final Collection<Object> values) {
		setValues(values == null ? null : values.toArray());
	}

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
}
