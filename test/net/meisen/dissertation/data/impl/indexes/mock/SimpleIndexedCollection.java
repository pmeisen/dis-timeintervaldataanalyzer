package net.meisen.dissertation.data.impl.indexes.mock;

import java.util.Collection;

import net.meisen.dissertation.models.impl.indexes.IndexKeyDefinition;
import net.meisen.dissertation.models.impl.indexes.IndexedCollection;

/**
 * A simple mock of a {@code IndexedCollection}.
 * 
 * @author pmeisen
 * 
 */
public class SimpleIndexedCollection extends IndexedCollection {

	/**
	 * Simple has a simple constructor.
	 * 
	 * @param keyDefinition
	 *            the {@code IndexKeyDefinition}
	 */
	public SimpleIndexedCollection(final IndexKeyDefinition keyDefinition) {
		super(keyDefinition);
	}

	@Override
	public boolean containsObject(final Object object) {
		return false;
	}

	@Override
	public boolean addObject(final Object object) {
		return false;
	}

	@Override
	public void removeAll() {
		// nothing to do just a mock
	}

	@Override
	public Object getObject(final Object... keys) {
		return null;
	}

	@Override
	public Collection<Object> getAll() {
		return null;
	}

	@Override
	public void removeObject(Object object) {
		// nothing to do just a mock
	}

	@Override
	public int size() {
		return 0;
	}
}
