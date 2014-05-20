package net.meisen.dissertation.model.data.metadata;

import java.util.Collection;
import java.util.Iterator;

import net.meisen.general.genmisc.collections.MultiMap;

public class MetaDataCollection implements Iterable<IMetaData> {
	private final MultiMap<String, IMetaData> metaData;

	public MetaDataCollection() {
		this.metaData = new MultiMap<String, IMetaData>();
	}

	public void addMetaData(final IMetaData metaData) {
		this.metaData.put(metaData.getDescriptorModelId(), metaData);
	}

	public void setMetaData(final Collection<IMetaData> metaData) {
		this.metaData.clear();

		for (final IMetaData md : metaData) {
			addMetaData(md);
		}
	}

	@Override
	public String toString() {

		final Iterator<String> i = metaData.keySet().iterator();
		if (!i.hasNext()) {
			return "{}";
		}

		final StringBuilder sb = new StringBuilder();
		sb.append('{');
		for (;;) {
			final String key = i.next();
			final Collection<IMetaData> value = metaData.getAll(key);
			sb.append(value.toString());

			if (!i.hasNext()) {
				return sb.append('}').toString();
			}
			sb.append(", ");
		}

	}

	@Override
	public Iterator<IMetaData> iterator() {
		return metaData.values().iterator();
	}
}
