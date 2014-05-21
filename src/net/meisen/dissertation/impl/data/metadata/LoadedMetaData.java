package net.meisen.dissertation.impl.data.metadata;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.meisen.dissertation.model.data.metadata.IIdentifiedMetaData;

public class LoadedMetaData implements IIdentifiedMetaData {

	private final Map<Object, Object> values;
	private final String descriptorModelId;

	public LoadedMetaData(final String descriptorModelId) {
		this.values = new HashMap<Object, Object>();
		this.descriptorModelId = descriptorModelId;
	}

	@Override
	public Collection<Object> getValues() {
		return values.values();
	}

	@Override
	public Map<Object, Object> getIdentifiedValues() {
		return values;
	}

	public void addValues(final Map<Object, Object> values) {
		if (values == null) {
			return;
		}

		for (final Entry<Object, Object> value : values.entrySet()) {
			addValue(value.getKey(), value.getValue());
		}
	}

	public void addValue(final Object key, final Object value) {
		this.values.put(key, value);
	}

	public void setValues(final Map<Object, Object> values) {
		this.values.clear();
		addValues(values);
	}

	@Override
	public String getDescriptorModelId() {
		return descriptorModelId;
	}

	@Override
	public String toString() {
		return getDescriptorModelId() + ": " + values;
	}

	@Override
	public int size() {
		return values.size();
	}
}
