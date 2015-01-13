package net.meisen.dissertation.impl.data.metadata;

import java.util.Collection;
import java.util.Iterator;

import net.meisen.dissertation.model.data.metadata.IMetaData;
import net.meisen.dissertation.model.data.metadata.IMetaDataCollection;

/**
 * A wrapper for a {@code IMetaDataCollection} to ensure no modification.
 * 
 * @author pmeisen
 * 
 */
public class ReadOnlyMetaDataCollection implements IMetaDataCollection {
	private final IMetaDataCollection collection;

	/**
	 * Constructor which defines the {@code collection} to be wrapped.
	 * 
	 * @param collection
	 *            the {@code collection} to be wrapped
	 */
	public ReadOnlyMetaDataCollection(final IMetaDataCollection collection) {
		this.collection = collection;
	}

	@Override
	public Iterator<IMetaData> iterator() {
		return collection.iterator();
	}

	@Override
	public void addMetaData(final Collection<IMetaData> metaData) {
		throw new UnsupportedOperationException(
				"Read-only does not support this method.");
	}

	@Override
	public void addMetaData(final IMetaData metaData) {
		throw new UnsupportedOperationException(
				"Read-only does not support this method.");
	}

	@Override
	public void setMetaData(final Collection<IMetaData> metaData) {
		throw new UnsupportedOperationException(
				"Read-only does not support this method.");
	}

	@Override
	public void add(final IMetaDataCollection collection) {
		throw new UnsupportedOperationException(
				"Read-only does not support this method.");
	}

	@Override
	public void clear() {
		throw new UnsupportedOperationException(
				"Read-only does not support this method.");
	}

	@Override
	public int size() {
		return collection.size();
	}

	@Override
	public int size(final String descriptorModelId) {
		return collection.size(descriptorModelId);
	}

	@Override
	public int sizeOfValues(final String descriptorModelId) {
		return collection.sizeOfValues(descriptorModelId);
	}

	@Override
	public Collection<IMetaData> get(final String descriptorModelId) {
		return collection.get(descriptorModelId);
	}

	@Override
	public String toString() {
		return collection.toString();
	}
}
