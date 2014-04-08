package net.meisen.dissertation.impl.indexes;

import java.util.Collection;
import java.util.Map;

import net.meisen.dissertation.model.indexes.IndexKey;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.BaseIndexedCollection;

/**
 * An implementation of a {@code BaseIndexedCollection} which uses a {@code Map} in
 * the background.
 * 
 * @author pmeisen
 * 
 * @see BaseIndexedCollection
 * 
 */
public class MapIndexedCollection extends BaseIndexedCollection {
	private Map<IndexKey<?>, Object> index;

	/**
	 * Constructor to create an index based on a {@code Map} implementation.
	 * 
	 * @param key
	 *            the definition of the {@code IndexKey}
	 * @param mapClass
	 *            the {@code Map} class
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public MapIndexedCollection(final IndexKeyDefinition key,
			final Class<? extends Map> mapClass) {
		super(key);

		try {
			this.index = (Map<IndexKey<?>, Object>) mapClass.newInstance();
		} catch (final Exception e) {
			throw new IllegalArgumentException("The specified mapClass '"
					+ mapClass.getName() + "' cannot be instantiated.", e);
		}
	}

	@Override
	public boolean containsObject(final Object object) {
		final IndexKey<?> key = getKeyDefinition().getKey(object);
		return index.containsKey(key);
	}

	@Override
	public boolean addObject(final Object object) {
		final IndexKey<?> key = getKeyDefinition().getKey(object);
		final Object oldObject = index.put(key, object);

		if (oldObject != null) {
			// we create the oldKey again, just to be sure that GC can work
			// who knows what is bound to a key
			final IndexKey<?> oldKey = getKeyDefinition().getKey(oldObject);
			index.put(oldKey, oldObject);
			return false;
		} else {
			return true;
		}
	}

	@Override
	public void removeObject(Object object) {
		final IndexKey<?> key = getKeyDefinition().getKey(object);
		index.remove(key);
	}

	@Override
	public void removeAll() {
		index.clear();
	}

	@Override
	public String toString() {
		return "HashMapIndex (" + getKeyDefinition() + ")";
	}

	@Override
	public Object getObject(final Object... values) {
		return index.get(getKeyDefinition().generateKeyFromValues(values));
	}

	@Override
	public Collection<Object> getAll() {
		return index.values();
	}

	@Override
	public int size() {
		return index.size();
	}
}
