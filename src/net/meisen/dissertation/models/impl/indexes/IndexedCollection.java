package net.meisen.dissertation.models.impl.indexes;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import net.meisen.dissertation.models.IIndexedCollection;

/**
 * A abstract implementation of an {@code IndexedCollection}.
 * 
 * @author pmeisen
 * 
 * @see IIndexedCollection
 * 
 */
public abstract class IndexedCollection implements IIndexedCollection {
	private final IndexKeyDefinition keyDefinition;

	/**
	 * Constructor which specifies the index's key.
	 * 
	 * @param keyDefinition
	 *            the {@code IndexKeyDefinition} to be used
	 */
	public IndexedCollection(final IndexKeyDefinition keyDefinition) {
		this.keyDefinition = keyDefinition;
	}

	/**
	 * Gets the {@code IndexKeyDefinition} for the collection.
	 * 
	 * @return the {@code IndexKeyDefinition}
	 */
	public IndexKeyDefinition getKeyDefinition() {
		return keyDefinition;
	}

	@Override
	public abstract boolean containsObject(final Object object);

	@Override
	public abstract boolean addObject(final Object object);

	@Override
	public abstract void removeAll();

	@Override
	public abstract Object getObject(final Object... keys);

	@Override
	public abstract Collection<?> getAll();

	@Override
	public abstract void removeObject(final Object object);

	@Override
	public boolean supports(final Object... values) {
		return getKeyDefinition().matches(values);
	}

	/**
	 * Adds the specified objects to the collection.
	 * 
	 * @param objects
	 *            the objects to be added
	 * 
	 * @return a {@code List} of ignored objects, i.e. objects not added
	 */
	public List<Object> addObjects(final Collection<Object> objects) {
		return addObjects(objects.toArray());
	}

	/**
	 * Adds the specified objects to the collection.
	 * 
	 * @param objects
	 *            the objects to be added
	 * 
	 * @return a {@code List} of ignored objects, i.e. objects not added
	 */
	public List<Object> addObjects(final Object... objects) {
		final List<Object> ignoredObjects = new ArrayList<Object>();

		for (final Object object : objects) {
			if (!addObject(object)) {
				ignoredObjects.add(object);
			}
		}

		return ignoredObjects;
	}

	@Override
	public String toString() {
		return getClass().getName() + " (" + getKeyDefinition() + ")";
	}
}
