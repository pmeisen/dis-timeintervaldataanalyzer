package net.meisen.dissertation.impl.data.metadata;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.meisen.dissertation.model.data.metadata.IIdentifiedMetaData;
import net.meisen.general.genmisc.types.Objects;

/**
 * {@code IdentifiedMetaData} which was loaded from a persisted state.
 * 
 * @author pmeisen
 * 
 * @see IIdentifiedMetaData
 * 
 */
public class LoadedMetaData implements IIdentifiedMetaData {

	private final Map<Object, Object> values;
	private final String descriptorModelId;

	/**
	 * Default constructor which specifies the {@code descriptorModelId} the
	 * meta-data is defined for.
	 * 
	 * @param descriptorModelId
	 *            the {@code descriptorModelId} the meta-data is defined for
	 */
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

	/**
	 * Adds the specified key-value pairs to {@code this}.
	 * 
	 * @param values
	 *            the key-value pairs to be added
	 */
	public void addValues(final Map<Object, Object> values) {
		if (values == null) {
			return;
		}

		for (final Entry<Object, Object> value : values.entrySet()) {
			addValue(value.getKey(), value.getValue());
		}
	}

	/**
	 * Adds a key-value pair to {@code this}.
	 * 
	 * @param key
	 *            the key of the value to be added
	 * @param value
	 *            the value to be added
	 */
	public void addValue(final Object key, final Object value) {
		this.values.put(key, value);
	}

	/**
	 * Sets the specified key-value pairs of {@code this}. In contrast to
	 * adding, setting removes all other already pairs prior to adding the
	 * specified {@code values}.
	 * 
	 * @param values
	 *            the key-value pairs to be set
	 */
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

	@Override
	public boolean equals(final Object obj) {
		if (obj == this) {
			return true;
		} else if (obj instanceof LoadedMetaData) {
			final LoadedMetaData lmd = (LoadedMetaData) obj;
			return Objects.equals(lmd.descriptorModelId, descriptorModelId)
					&& Objects.equals(lmd.values, values);
		} else {
			return false;
		}
	}
}
