package net.meisen.dissertation.impl.data.metadata;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import net.meisen.dissertation.exceptions.MetaDataCollectionException;
import net.meisen.dissertation.model.data.metadata.IIdentifiedMetaData;
import net.meisen.dissertation.model.data.metadata.IMetaData;
import net.meisen.dissertation.model.data.metadata.IMetaDataCollection;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Objects;

/**
 * A collection of different {@code MetaData} instances. The collection units
 * all the {@code MetaData} defined.
 * 
 * @author pmeisen
 * 
 */
public class DescriptorMetaDataCollection implements IMetaDataCollection {
	private final ReentrantReadWriteLock lock;
	private final Map<String, LoadedMetaData> metaData;

	/**
	 * Default constructor.
	 */
	public DescriptorMetaDataCollection() {
		this.lock = new ReentrantReadWriteLock();
		this.metaData = new HashMap<String, LoadedMetaData>();
	}

	/**
	 * Adds the specified {@code Descriptor} to the cache.
	 * 
	 * @param desc
	 *            the {@code Descriptor} to be added
	 */
	public void addDescriptor(final Descriptor<?, ?, ?> desc) {
		final LoadedMetaData metaData = new LoadedMetaData(desc.getModelId());
		metaData.addValue(desc.getId(), desc.getValue());

		this.addMetaData(metaData);
	}

	@Override
	public void addMetaData(final Collection<IMetaData> metaData)
			throws ForwardedRuntimeException {
		if (metaData == null) {
			return;
		}

		// all each element
		for (final IMetaData md : metaData) {
			addMetaData(md);
		}
	}

	/**
	 * Method used to check if the {@code MetaDataCollection} contains the
	 * meta-information of the specified {@code Descriptor}.
	 * 
	 * @param desc
	 *            the {@code Descriptor} to be checked
	 * 
	 * @return {@code true} if {@code desc} is already defined within, otherwise
	 *         {@code false}
	 */
	public boolean contains(final Descriptor<?, ?, ?> desc) {
		this.lock.readLock().lock();

		try {
			final LoadedMetaData current = metaData.get(desc.getModelId());
			return contains(current, desc.getId(), desc.getValue());
		} finally {
			this.lock.readLock().unlock();
		}
	}

	/**
	 * Checks if the specified {@code metaData} contains an information with the
	 * specified {@code id} and {@code value}.
	 * 
	 * @param metaData
	 *            the meta-data to look into
	 * @param id
	 *            the identifier to search for
	 * @param value
	 *            the value to search for
	 * 
	 * @return (@code true) if the {@code MetaData} contains the specified
	 *         entry, otherwise (@code false)
	 */
	protected boolean contains(final LoadedMetaData metaData, final Object id,
			final Object value) {
		if (metaData == null) {
			return false;
		}

		final Map<Object, Object> curValues = metaData.getIdentifiedValues();
		if (curValues.containsKey(id)) {
			final Object curValue = curValues.get(id);
			return Objects.equals(curValue, value);
		} else {
			return false;
		}
	}

	@Override
	public void addMetaData(final IMetaData metaData)
			throws ForwardedRuntimeException {
		if (metaData == null) {
			throw new ForwardedRuntimeException(
					MetaDataCollectionException.class, 1002);
		} else if (metaData instanceof IIdentifiedMetaData == false) {
			throw new ForwardedRuntimeException(
					MetaDataCollectionException.class, 1001, metaData
							.getClass().getSimpleName(), metaData);
		}
		final IIdentifiedMetaData idMetaData = (IIdentifiedMetaData) metaData;
		final String descModelId = idMetaData.getDescriptorModelId();

		lock.writeLock().lock();
		try {
			final LoadedMetaData curMetaData = this.metaData.get(descModelId);

			if (curMetaData == null || curMetaData.size() == 0) {
				if (idMetaData instanceof LoadedMetaData) {
					this.metaData.put(descModelId, (LoadedMetaData) idMetaData);
				} else {
					final LoadedMetaData nmd = new LoadedMetaData(descModelId);
					nmd.addValues(idMetaData.getIdentifiedValues());
					this.metaData.put(descModelId, nmd);
				}
			} else {
				combineMetaData(descModelId, curMetaData, idMetaData);
			}
		} finally {
			lock.writeLock().unlock();
		}
	}

	/**
	 * This method is used to combine the current {@code Collection} of
	 * meta-data associated with a {@code DescriptorModel} with the newly added
	 * once.
	 * 
	 * @param descModelId
	 *            the identifier of the {@code DescriptorModel}
	 * @param current
	 *            the current meta-data
	 * @param added
	 *            the meta-data to be added
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the specified meta-data is invalid
	 */
	protected void combineMetaData(final String descModelId,
			final LoadedMetaData current, final IIdentifiedMetaData added)
			throws ForwardedRuntimeException {
		if (added == null) {
			return;
		}

		/*
		 * We have identifiers to be added and we already have an identifier
		 * set.
		 * 
		 * currentDefMetaData might be null, or not. Therefore we have to
		 * consider it.
		 */
		final Map<Object, Object> curValues = current.getIdentifiedValues();
		final Map<Object, Object> addedValues = added.getIdentifiedValues();

		// add the values, but make sure there is nothing modified
		for (final Entry<Object, Object> e : addedValues.entrySet()) {
			final Object k = e.getKey();
			final Object v = e.getValue();

			if (curValues.containsKey(k)) {
				final Object curValue = curValues.get(k);
				if (!Objects.equals(curValue, v)) {
					throw new ForwardedRuntimeException(
							MetaDataCollectionException.class, 1000, k,
							curValue, v);
				}
			} else {
				current.addValue(k, v);
			}
		}

		// create a new id meta-data set
		current.addValues(addedValues);
	}

	@Override
	public void setMetaData(final Collection<IMetaData> metaData)
			throws ForwardedRuntimeException {
		lock.writeLock().lock();
		try {
			clear();
			addMetaData(metaData);
		} finally {
			lock.writeLock().unlock();
		}
	}

	@Override
	public void add(final IMetaDataCollection collection) {
		if (collection == null) {
			return;
		}
		final Iterator<IMetaData> it = collection.iterator();
		while (it.hasNext()) {
			this.addMetaData(it.next());
		}
	}

	@Override
	public String toString() {
		lock.readLock().lock();
		try {
			return metaData.toString();
		} finally {
			lock.readLock().unlock();
		}
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == this) {
			return true;
		} else if (obj instanceof DescriptorMetaDataCollection) {
			final DescriptorMetaDataCollection col = (DescriptorMetaDataCollection) obj;
			return metaData.equals(col.metaData);
		} else {
			return false;
		}
	}

	@Override
	public Iterator<IMetaData> iterator() {
		lock.readLock().lock();
		try {
			final List<IMetaData> copy = new ArrayList<IMetaData>(
					metaData.values());
			return copy.iterator();
		} finally {
			lock.readLock().unlock();
		}
	}

	@Override
	public void clear() {
		lock.writeLock().lock();
		try {
			metaData.clear();
		} finally {
			lock.writeLock().unlock();
		}
	}

	@Override
	public int size() {
		lock.readLock().lock();
		try {
			return metaData.size();
		} finally {
			lock.readLock().unlock();
		}
	}

	@Override
	public int size(final String descriptorModelId) {
		lock.readLock().lock();
		try {
			final Collection<IMetaData> metaData = get(descriptorModelId);
			return metaData == null ? 0 : metaData.size();
		} finally {
			lock.readLock().unlock();
		}
	}

	@Override
	public Collection<IMetaData> get(final String descriptorModelId) {
		final List<IMetaData> data = new ArrayList<IMetaData>();

		lock.readLock().lock();
		try {
			final LoadedMetaData metaData = this.metaData
					.get(descriptorModelId);
			if (metaData != null) {
				data.add(metaData);
			}
			return data;
		} finally {
			lock.readLock().unlock();
		}
	}

	@Override
	public int sizeOfValues(final String descriptorModelId) {
		lock.readLock().lock();

		try {
			final Collection<IMetaData> metaData = get(descriptorModelId);
			if (metaData == null) {
				return 0;
			}

			int count = 0;
			for (final IMetaData md : metaData) {
				count += md == null ? 0 : md.size();
			}

			return count;
		} finally {
			lock.readLock().unlock();
		}
	}

	/**
	 * Gets the (@code MetaData} which contains the value for the specified
	 * {@code descModelId}.
	 * 
	 * @param descModelId
	 *            the identifier of the {@code DescriptorModel}
	 * @param value
	 *            the value of the meta-data to be retrieved
	 * 
	 * @return the meta-data containing the specified value
	 */
	@SuppressWarnings("unchecked")
	public <T extends IMetaData> T get(final String descModelId,
			final Object value) {
		lock.readLock().lock();

		try {
			final Collection<IMetaData> mds = get(descModelId);

			for (final IMetaData md : mds) {
				if (md.getValues().contains(value)) {
					return (T) md;
				}
			}

			return null;
		} finally {
			lock.readLock().unlock();
		}
	}
}
