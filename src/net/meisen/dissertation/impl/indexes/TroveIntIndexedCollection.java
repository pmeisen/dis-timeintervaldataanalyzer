package net.meisen.dissertation.impl.indexes;

import gnu.trove.map.hash.TIntObjectHashMap;

import java.util.Arrays;
import java.util.Collection;

import net.meisen.dissertation.model.indexes.BaseIndexedCollection;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;

/**
 * An implementation of a {@code BaseIndexedCollection} based on a
 * {@code TIntObjectHashMap}.
 * 
 * @author pmeisen
 * 
 */
public class TroveIntIndexedCollection extends BaseIndexedCollection {

	private final TIntObjectHashMap<Object> troveMap;

	/**
	 * Constructor which specifies the index's key.
	 * 
	 * @param keyDefinition
	 *            the {@code IndexKeyDefinition} to be used
	 */
	public TroveIntIndexedCollection(final IndexKeyDefinition keyDefinition) {
		super(keyDefinition);
		
		if (!keyDefinition.isSingleTypedKey(Integer.class)) {
			throw new IllegalArgumentException(
					"The key must be a single Integer");
		}
		
		troveMap = new TIntObjectHashMap<Object>();
	}

	@Override
	public boolean containsObject(final Object object) {
		final int key = getKeyDefinition().getIntKey(object);
		return troveMap.contains(key);
	}

	@Override
	public boolean addObject(final Object object) {
		final int key = getKeyDefinition().getIntKey(object);
		final Object oldObject = troveMap.put(key, object);

		if (oldObject != null) {
			troveMap.put(key, oldObject);
			return false;
		} else {
			return true;
		}
	}

	@Override
	public void removeObject(final Object object) {
		final int key = getKeyDefinition().getIntKey(object);
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

		final int key = getKeyDefinition().generateIntKeyFromValue(values[0]);
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
