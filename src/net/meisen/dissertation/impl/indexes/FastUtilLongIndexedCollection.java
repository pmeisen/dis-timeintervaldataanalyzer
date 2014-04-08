package net.meisen.dissertation.impl.indexes;

import java.util.Collection;

import it.unimi.dsi.fastutil.longs.Long2ObjectOpenHashMap;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.BaseIndexedCollection;

/**
 * An implementation of a {@code BaseIndexedCollection} based on a
 * {@code FastUtilLongIndexedCollection}.
 * 
 * @author pmeisen
 * 
 */
public class FastUtilLongIndexedCollection extends BaseIndexedCollection {

	private final Long2ObjectOpenHashMap<Object> fastUtilMap;

	/**
	 * Constructor which specifies the index's key.
	 * 
	 * @param keyDefinition
	 *            the {@code IndexKeyDefinition} to be used
	 */
	public FastUtilLongIndexedCollection(final IndexKeyDefinition keyDefinition) {
		super(keyDefinition);

		if (!keyDefinition.isSingleTypedKey(Long.class)) {
			throw new IllegalArgumentException("The key must be a single Long");
		}

		fastUtilMap = new Long2ObjectOpenHashMap<Object>();
	}

	@Override
	public boolean containsObject(final Object object) {
		final long key = getKeyDefinition().getLongKey(object);
		return fastUtilMap.containsKey(key);
	}

	@Override
	public boolean addObject(final Object object) {
		final long key = getKeyDefinition().getLongKey(object);
		final Object oldObject = fastUtilMap.put(key, object);

		if (oldObject != null) {
			fastUtilMap.put(key, oldObject);
			return false;
		} else {
			return true;
		}
	}
	
	@Override
	public void removeObject(final Object object) {
		final long key = getKeyDefinition().getLongKey(object);
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

		final long key = getKeyDefinition().generateLongKeyFromValue(values[0]);
		return fastUtilMap.get(key);
	}
	
	@Override
	public Collection<Object> getAll() {
		return  fastUtilMap.values();
	}
	
	@Override
	public int size() {
		return fastUtilMap.size();
	}
}
