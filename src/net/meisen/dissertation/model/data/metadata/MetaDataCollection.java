package net.meisen.dissertation.model.data.metadata;

import java.util.Collection;
import java.util.Iterator;

import net.meisen.general.genmisc.collections.MultiMap;

/**
 * A collection of different {@code MetaData} instances. The collection units
 * all the {@code MetaData} defined.
 * 
 * @author pmeisen
 * 
 */
public class MetaDataCollection implements Iterable<IMetaData> {
	private final MultiMap<String, IMetaData> metaData;

	/**
	 * Default constructor.
	 */
	public MetaDataCollection() {
		this.metaData = new MultiMap<String, IMetaData>();
	}

	/**
	 * Adds all the {@code MetaData} to {@code this}.
	 * 
	 * @param metaData
	 *            the collection of {@code MetaData} to be added
	 */
	public void addMetaData(final Collection<IMetaData> metaData) {
		if (metaData == null) {
			return;
		}

		// all each element
		for (final IMetaData md : metaData) {
			addMetaData(md);
		}
	}

	/**
	 * Adds the {@code MetaData} to {@code this}.
	 * 
	 * @param metaData
	 *            the {@code MetaData} to be added
	 */
	public void addMetaData(final IMetaData metaData) {
		this.metaData.put(metaData.getDescriptorModelId(), metaData);
	}

	/**
	 * Sets the {@code MetaData} to be collected.
	 * 
	 * @param metaData
	 *            the {@code MetaData} to be collected
	 */
	public void setMetaData(final Collection<IMetaData> metaData) {
		this.metaData.clear();
		addMetaData(metaData);
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
