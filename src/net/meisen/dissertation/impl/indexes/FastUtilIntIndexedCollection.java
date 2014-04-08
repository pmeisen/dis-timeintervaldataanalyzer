package net.meisen.dissertation.impl.indexes;

import it.unimi.dsi.fastutil.ints.Int2ObjectOpenHashMap;

import java.util.Collection;

import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.BaseIndexedCollection;

/**
 * An implementation of a {@code BaseIndexedCollection} based on a
 * {@code Int2ObjectOpenHashMap}.
 * 
 * @author pmeisen
 * 
 */
public class FastUtilIntIndexedCollection extends BaseIndexedCollection {

	private final Int2ObjectOpenHashMap<Object> fastUtilMap;

	/**
	 * Constructor which specifies the index's key.
	 * 
	 * @param keyDefinition
	 *            the {@code IndexKeyDefinition} to be used
	 */
	public FastUtilIntIndexedCollection(final IndexKeyDefinition keyDefinition) {
		super(keyDefinition);

		if (!keyDefinition.isSingleTypedKey(Integer.class)) {
			throw new IllegalArgumentException(
					"The key must be a single Integer");
		}

		fastUtilMap = new Int2ObjectOpenHashMap<Object>();
	}

	@Override
	public boolean containsObject(final Object object) {
		final int key = getKeyDefinition().getIntKey(object);
		return fastUtilMap.containsKey(key);
	}

	@Override
	public boolean addObject(final Object object) {
		final int key = getKeyDefinition().getIntKey(object);
		final Object oldObject = fastUtilMap.put(key, object);

		if (oldObject != null) {
			fastUtilMap.put(key, oldObject);
			return false;
		} else {
			return true;
		}
	}
	
	@Override
	public void removeObject(Object object) {
		final int key = getKeyDefinition().getIntKey(object);
		fastUtilMap.remove(key);
	}

	@Override
	public void removeAll() {
		fastUtilMap.clear();
	}

	@Override
	public Object getObject(final Object... values) {
		if (values == null || values.length != 1) {
			throw new IllegalArgumentException(
					"The values cannot be null nor can it be multiple values.");
		}

		final int key = getKeyDefinition().generateIntKeyFromValue(values[0]);
		return fastUtilMap.get(key);
	}

	@Override
	public Collection<Object> getAll() {
		return fastUtilMap.values();
	}

	@Override
	public int size() {
		return fastUtilMap.size();
	}
}
