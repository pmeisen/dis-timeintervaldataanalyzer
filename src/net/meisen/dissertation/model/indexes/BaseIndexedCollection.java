package net.meisen.dissertation.model.indexes;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;


/**
 * A abstract implementation of an {@code BaseIndexedCollection}.
 * 
 * @author pmeisen
 * 
 * @see IIndexedCollection
 * 
 */
public abstract class BaseIndexedCollection implements IIndexedCollection {
	private final IndexKeyDefinition keyDefinition;

	/**
	 * Constructor which specifies the index's key.
	 * 
	 * @param keyDefinition
	 *            the {@code IndexKeyDefinition} to be used
	 */
	public BaseIndexedCollection(final IndexKeyDefinition keyDefinition) {
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
