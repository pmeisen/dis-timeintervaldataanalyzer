package net.meisen.dissertation.data.impl.indexes;

import java.util.Arrays;
import java.util.Collection;

import gnu.trove.map.hash.TLongObjectHashMap;
import net.meisen.dissertation.models.impl.indexes.IndexKeyDefinition;
import net.meisen.dissertation.models.impl.indexes.IndexedCollection;

/**
 * An implementation of a {@code IndexedCollection} based on a
 * {@code TLongObjectHashMap}.
 * 
 * @author pmeisen
 * 
 */
public class TroveLongIndexedCollection extends IndexedCollection {

	private final TLongObjectHashMap<Object> troveMap;

	/**
	 * Constructor which specifies the index's key.
	 * 
	 * @param keyDefinition
	 *            the {@code IndexKeyDefinition} to be used
	 */
	public TroveLongIndexedCollection(final IndexKeyDefinition keyDefinition) {
		super(keyDefinition);

		if (!keyDefinition.isSingleTypedKey(Long.class)) {
			throw new IllegalArgumentException("The key must be a single Long");
		}

		troveMap = new TLongObjectHashMap<Object>();
	}

	@Override
	public boolean containsObject(final Object object) {
		final long key = getKeyDefinition().getLongKey(object);
		return troveMap.contains(key);
	}

	@Override
	public boolean addObject(final Object object) {
		final long key = getKeyDefinition().getLongKey(object);
		final Object oldObject = troveMap.put(key, object);

		if (oldObject != null) {
			troveMap.put(key, oldObject);
			return false;
		} else {
			return true;
		}
	}

	@Override
	public void removeObject(Object object) {
		final long key = getKeyDefinition().getLongKey(object);
		troveMap.remove(key);
	}

	@Override
	public void removeAll() {
		troveMap.clear();
	}

	@Override
	public Object getObject(final Object... values) {
		if (values == null || values.length != 1) {
			throw new IllegalArgumentException(
					"The values cannot be null nor can it be multiple values.");
		}

		final long key = getKeyDefinition().generateLongKeyFromValue(values[0]);
		return troveMap.get(key);
	}

	@Override
	public Collection<Object> getAll() {
		return Arrays.asList(troveMap.values());
	}
	
	@Override
	public int size() {
		return troveMap.size();
	}
}
