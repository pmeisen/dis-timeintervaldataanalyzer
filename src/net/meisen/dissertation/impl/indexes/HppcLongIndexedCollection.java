package net.meisen.dissertation.impl.indexes;

import java.util.Arrays;
import java.util.Collection;

import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.BaseIndexedCollection;

import com.carrotsearch.hppc.LongObjectOpenHashMap;

/**
 * An implementation of a {@code BaseIndexedCollection} based on a
 * {@code LongObjectOpenHashMap}.
 * 
 * @author pmeisen
 * 
 */
public class HppcLongIndexedCollection extends BaseIndexedCollection {

	private final LongObjectOpenHashMap<Object> hppcMap;

	/**
	 * Constructor which specifies the index's key.
	 * 
	 * @param keyDefinition
	 *            the {@code IndexKeyDefinition} to be used
	 */
	public HppcLongIndexedCollection(final IndexKeyDefinition keyDefinition) {
		super(keyDefinition);

		if (!keyDefinition.isSingleTypedKey(Long.class)) {
			throw new IllegalArgumentException("The key must be a single Long");
		}

		hppcMap = new LongObjectOpenHashMap<Object>();
	}

	@Override
	public boolean containsObject(final Object object) {
		final long key = getKeyDefinition().getLongKey(object);
		return hppcMap.containsKey(key);
	}

	@Override
	public boolean addObject(final Object object) {
		final long key = getKeyDefinition().getLongKey(object);
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
		final long key = getKeyDefinition().getLongKey(object);
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

		final long key = getKeyDefinition().generateLongKeyFromValue(values[0]);
		return hppcMap.get(key);
	}

	@Override
	public Collection<Object> getAll() {
		return Arrays.asList(hppcMap.values().toArray());
	}
	
	@Override
	public int size() {
		return hppcMap.size();
	}
}
