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
		clear();
		addMetaData(metaData);
	}

	/**
	 * Adds the {@code MetaData} of the passed {@code collection} to
	 * {@code this}.
	 * 
	 * @param collection
	 *            the {@code MetaDataCollection} to add the {@code MetaData}
	 *            from
	 */
	public void add(final MetaDataCollection collection) {
		if (collection == null || collection.metaData == null) {
			return;
		}
		addMetaData(collection.metaData.values());
	}

	@Override
	public String toString() {
		return metaData.toString();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == this) {
			return true;
		} else if (obj instanceof MetaDataCollection) {
			final MetaDataCollection col = (MetaDataCollection) obj;
			return metaData.equals(col.metaData);
		} else {
			return false;
		}
	}

	@Override
	public Iterator<IMetaData> iterator() {
		return metaData.values().iterator();
	}

	/**
	 * Removes all the values from the collection.
	 */
	public void clear() {
		metaData.clear();
	}

	/**
	 * Get the amount of {@code DescriptorModel} associations.
	 * 
	 * @return the amount of {@code DescriptorModel} associations
	 */
	public int size() {
		return metaData.size();
	}

	/**
	 * Get the amount of {@code MetaData} instances associated to the specified
	 * {@code DescriptorModel}.
	 * 
	 * @param descriptorModelId
	 *            the identifier of the {@code DescriptorModel} to get the
	 *            amount of {@code MetaData} instances from
	 * 
	 * @return the amount of {@code MetaData} instances associated to the
	 *         specified {@code DescriptorModel}
	 */
	public int size(final String descriptorModelId) {
		final Collection<IMetaData> metaData = get(descriptorModelId);

		return metaData == null ? 0 : metaData.size();
	}

	/**
	 * Gets all the defined meta-data for the specified
	 * {@code descriptorModelId}.
	 * 
	 * @param descriptorModelId
	 *            the identifier of the {@code DescriptorModel} to get the
	 *            meta-data for
	 * 
	 * @return a collection of all the meta-data defined for the specified
	 *         {@code descriptorModelId}
	 */
	public Collection<IMetaData> get(final String descriptorModelId) {
		return this.metaData.getAll(descriptorModelId);
	}

	/**
	 * Determines the amount of values available for a specified
	 * {@code descriptorModelId}.
	 * 
	 * @param descriptorModelId
	 *            the identifier of the {@code DescriptorModel} to determine the
	 *            size for
	 * 
	 * @return the amount of values available for a specified
	 *         {@code descriptorModelId}
	 */
	public int sizeOfValues(final String descriptorModelId) {
		final Collection<IMetaData> metaData = get(descriptorModelId);
		if (metaData == null) {
			return 0;
		}

		int count = 0;
		for (final IMetaData md : metaData) {
			count += md == null ? 0 : md.size();
		}

		return count;
	}
}
