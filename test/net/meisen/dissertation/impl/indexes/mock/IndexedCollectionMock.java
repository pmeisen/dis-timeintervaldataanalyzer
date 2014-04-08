package net.meisen.dissertation.impl.indexes.mock;

import java.util.Collection;

import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.BaseIndexedCollection;

/**
 * Mock of a {@code BaseIndexedCollection}.
 * 
 * @author pmeisen
 */
public class IndexedCollectionMock extends BaseIndexedCollection {

	/**
	 * Needed constructor.
	 * 
	 * @param keyDefinition
	 *            the definition of the key
	 */
	public IndexedCollectionMock(final IndexKeyDefinition keyDefinition) {
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
	public void removeObject(final Object object) {
		// nothing to do
	}

	@Override
	public void removeAll() {
		// nothing to do
	}

	@Override
	public Object getObject(final Object... values) {
		return null;
	}

	@Override
	public Collection<?> getAll() {
		return null;
	}

	@Override
	public int size() {
		return 0;
	}

}
