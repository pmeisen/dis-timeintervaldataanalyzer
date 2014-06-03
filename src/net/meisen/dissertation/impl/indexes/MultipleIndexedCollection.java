package net.meisen.dissertation.impl.indexes;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import net.meisen.dissertation.model.indexes.IIndexedCollection;
import net.meisen.dissertation.model.indexes.IMultipleKeySupport;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.BaseIndexedCollection;
import net.meisen.dissertation.model.indexes.IndexedCollectionDefinition;

/**
 * A {@code MultipleIndexedCollection} implements the {@code MultipleKeySupport}
 * . This means that this collection can support several
 * {@code IndexKeyDefinitions} to add an object to the {@code BaseIndexedCollection}
 * .
 * 
 * @author pmeisen
 * 
 */
public class MultipleIndexedCollection extends BaseIndexedCollection implements
		IMultipleKeySupport {

	private final IIndexedCollection[] idx;
	private final int size;

	/**
	 * Constructor for only one {@code IndexKeyDefinition} and one
	 * {@code IndexedCollectionDefinition}.<br/>
	 * <br/>
	 * <b>Note:</b><br/>
	 * In that case the {@code BaseIndexedCollection} should be created directly.
	 * 
	 * @param keyDefinition
	 * @param collDefinition
	 * 
	 * @see IIndexedCollection
	 * @see IndexedCollectionDefinition
	 */
	public MultipleIndexedCollection(final IndexKeyDefinition keyDefinition,
			final IndexedCollectionDefinition collDefinition) {
		this(new IndexKeyDefinition[] { keyDefinition },
				new IndexedCollectionDefinition[] { collDefinition });
	}

	/**
	 * Creates a {@code MultipleIndexedCollection} using the specified
	 * {@code IndexKeyDefinitions} for the specified
	 * {@code IndexedCollectionDefinitions}.
	 * 
	 * @param keyDefinitions
	 *            the definitions of the {@code IndexKeyDefinition}
	 * @param collDefinitions
	 *            the definitions of the {@code IndexedCollectionDefinition}
	 *            associated to the keys
	 */
	public MultipleIndexedCollection(final IndexKeyDefinition[] keyDefinitions,
			final IndexedCollectionDefinition[] collDefinitions) {
		super(keyDefinitions == null || keyDefinitions.length == 0 ? null
				: keyDefinitions[0]);

		if (collDefinitions == null) {
			throw new NullPointerException(
					"The definitions of the collections cannot be null.");
		} else if (keyDefinitions.length != collDefinitions.length) {
			throw new IllegalArgumentException(
					"The size of the definitions must be equal.");
		} else {
			Class<?> objectClass = null;
			for (final IndexKeyDefinition keyDefinition : keyDefinitions) {
				final Class<?> keyObjectClass = keyDefinition.getObjectClass();
				if (objectClass == null) {
					objectClass = keyObjectClass;
				} else if (objectClass.equals(keyObjectClass)) {
					continue;
				} else {
					throw new IllegalArgumentException(
							"The objectClasses of all keys must be equal ('"
									+ objectClass.getName() + "' != '"
									+ keyObjectClass + "'");
				}
			}
		}
		this.size = keyDefinitions.length;

		// create the different indexes
		idx = new BaseIndexedCollection[this.size];
		for (int i = 0; i < this.size; i++) {
			idx[i] = collDefinitions[i].create(keyDefinitions[i]);
		}
	}

	@Override
	public boolean containsObject(final Object object) {
		boolean first = idx[0].containsObject(object);

		// check every index, even if one should be enough data quality is more
		// important than speed, if speed is important getObject should be used
		// and compared to {@code null}
		for (int i = 0; i < this.size; i++) {
			if (first != idx[i].containsObject(object)) {
				throw new IllegalStateException(
						"MultipleIndexedCollection is out of sync.");
			}
		}

		return first;
	}

	@Override
	public boolean addObject(final Object object) {

		for (int i = 0; i < this.size; i++) {
			if (!idx[i].addObject(object)) {

				// rollback the changes done so far
				for (int k = 0; k < i; k++) {
					idx[k].removeObject(object);
				}

				return false;
			}
		}

		return true;
	}

	@Override
	public void removeObject(final Object object) {
		for (int i = 0; i < this.size; i++) {
			idx[i].removeObject(object);
		}
	}

	@Override
	public void removeAll() {
		for (int i = 0; i < this.size; i++) {
			idx[i].removeAll();
		}
	}

	@Override
	public Object getObjectByDefNr(final int keyDefNr, final Object... values) {
		final IIndexedCollection idx = this.idx[keyDefNr];

		if (idx.supports(values)) {
			return getObject(idx, values);
		} else {
			throw new IllegalArgumentException(
					"The KeyDefinition '"
							+ keyDefNr
							+ "' is not valid for the values '"
							+ (values == null ? null : Arrays.asList(values)
									.toString()) + "'");
		}
	}
	
	@Override
	public Object getObject(final Object... values) {

		// get the matched key
		for (int i = 0; i < this.size; i++) {
			final IIndexedCollection idx = this.idx[i];

			// if a key matches we have it
			if (idx.supports(values)) {
				return getObject(idx, values);
			}
		}

		// no key matched
		throw new IllegalArgumentException(
				"Could not find any KeyDefinition for the values '"
						+ (values == null ? null : Arrays.asList(values)
								.toString()) + "'");
	}

	/**
	 * Gets the object associated to the specified keys.
	 * 
	 * @param idx
	 *            the {@code BaseIndexedCollection} to get the associated object
	 *            from
	 * @param values
	 *            the values to get the object for
	 * 
	 * @return the associated object
	 */
	protected Object getObject(final IIndexedCollection idx,
			final Object... values) {
		return idx.getObject(values);
	}

	/**
	 * Get the index used for the {@code IndexKey} at position {@code pos}.
	 * 
	 * @param pos
	 *            the position to get the {@code BaseIndexedCollection} for
	 * 
	 * @return the {@code BaseIndexedCollection} for the specified position
	 */
	public IIndexedCollection getIndexedCollection(final int pos) {
		return idx[pos];
	}

	/**
	 * Gets a {@code List} of all the {@code BaseIndexedCollection} of {@code this}
	 * {@code MultipleIndexedCollection}.
	 * 
	 * @return all the {@code BaseIndexedCollection} of {@code this}
	 *         {@code MultipleIndexedCollection}
	 */
	public List<IIndexedCollection> getAllIndexedCollection() {
		return Arrays.asList(idx);
	}

	@Override
	public Collection<?> getAll() {
		return idx[0].getAll();
	}

	@Override
	public String toString() {
		String res = getClass().getName() + " {";
		String sep = "";
		for (int i = 0; i < this.size; i++) {
			res += sep + idx[i].toString();
			if ("".equals(sep)) {
				sep = ", ";
			}
		}
		res += "}";

		return res;
	}
	
	@Override
	public int size() {
		return idx[0].size();
	}
}
