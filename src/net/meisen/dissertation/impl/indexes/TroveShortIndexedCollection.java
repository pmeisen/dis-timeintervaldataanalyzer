package net.meisen.dissertation.impl.indexes;

import gnu.trove.map.hash.TShortObjectHashMap;

import java.util.Arrays;
import java.util.Collection;

import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.BaseIndexedCollection;

/**
 * An implementation of a {@code BaseIndexedCollection} based on a
 * {@code TShortObjectHashMap}.
 * 
 * @author pmeisen
 * 
 */
public class TroveShortIndexedCollection extends BaseIndexedCollection {

	private final TShortObjectHashMap<Object> troveMap;

	/**
	 * Constructor which specifies the index's key.
	 * 
	 * @param keyDefinition
	 *            the {@code IndexKeyDefinition} to be used
	 */
	public TroveShortIndexedCollection(final IndexKeyDefinition keyDefinition) {
		super(keyDefinition);

		if (!keyDefinition.isSingleTypedKey(Short.class)) {
			throw new IllegalArgumentException("The key must be a single Short");
		}

		troveMap = new TShortObjectHashMap<Object>();
	}

	@Override
	public boolean containsObject(final Object object) {
		final short key = getKeyDefinition().getShortKey(object);
		return troveMap.contains(key);
	}

	@Override
	public boolean addObject(final Object object) {
		final short key = getKeyDefinition().getShortKey(object);
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
		final short key = getKeyDefinition().getShortKey(object);
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

		final short key = getKeyDefinition().generateShortKeyFromValue(values[0]);
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
