package net.meisen.dissertation.data.impl.indexes;

import java.util.Arrays;
import java.util.Collection;

import net.meisen.dissertation.models.impl.indexes.IndexKeyDefinition;
import net.meisen.dissertation.models.impl.indexes.IndexedCollection;

import com.carrotsearch.hppc.IntObjectOpenHashMap;

/**
 * An implementation of a {@code IndexedCollection} based on a
 * {@code IntObjectOpenHashMap}.
 * 
 * @author pmeisen
 * 
 */
public class HppcIntIndexedCollection extends IndexedCollection {

	private final IntObjectOpenHashMap<Object> hppcMap;

	/**
	 * Constructor which specifies the index's key.
	 * 
	 * @param keyDefinition
	 *            the {@code IndexKeyDefinition} to be used
	 */
	public HppcIntIndexedCollection(final IndexKeyDefinition keyDefinition) {
		super(keyDefinition);

		if (!keyDefinition.isSingleTypedKey(Integer.class)) {
			throw new IllegalArgumentException(
					"The key must be a single Integer");
		}

		hppcMap = new IntObjectOpenHashMap<Object>();
	}

	@Override
	public boolean containsObject(final Object object) {
		final int key = getKeyDefinition().getIntKey(object);
		return hppcMap.containsKey(key);
	}

	@Override
	public boolean addObject(final Object object) {
		final int key = getKeyDefinition().getIntKey(object);
		final Object oldObject = hppcMap.put(key, object);

		if (oldObject != null) {
			hppcMap.put(key, oldObject);
			return false;
		} else {
			return true;
		}
	}

	@Override
	public void removeObject(Object object) {
		final int key = getKeyDefinition().getIntKey(object);
		hppcMap.remove(key);
	}

	@Override
	public void removeAll() {
		hppcMap.clear();
	}

	@Override
	public Object getObject(final Object... values) {
		if (values == null || values.length != 1) {
			throw new IllegalArgumentException(
					"The values cannot be null nor can it be multiple values.");
		}

		final int key = getKeyDefinition().generateIntKeyFromValue(values[0]);
		return hppcMap.get(key);
	}

	@Override
	public Collection<Object> getAll() {
		return Arrays.asList(hppcMap.values().toArray());
	}
}
