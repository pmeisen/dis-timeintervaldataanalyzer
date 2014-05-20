package net.meisen.dissertation.model.indexes;

import java.util.Collection;

/**
 * An interface for an {@code BaseIndexedCollection}.
 * 
 * @author pmeisen
 * 
 */
public interface IIndexedCollection {

	/**
	 * Checks if the {@code object} is contained within the collection. If the
	 * object is needed afterwards it makes more sense to retrieve the object
	 * using {@link #getObject(Object...)} and check for {@code null}. If the
	 * object should be added it makes more sense (from a performance point of
	 * view) to add the object and check the returned value. For specific
	 * performance hints check the documentation for the concrete
	 * implementation.
	 * 
	 * @param object
	 *            the object to look for within the collection
	 * 
	 * @return {@code true} if the object is within the collection, otherwise
	 *         {@code false}
	 */
	public boolean containsObject(final Object object);

	/**
	 * Adds the specified object to the collection. If the object is already
	 * indexed, it will not be added again or override the old object, instead
	 * {@code false} will be returned.
	 * 
	 * @param object
	 *            the object to be added
	 * 
	 * @return {@code true} if the object was added, otherwise {@code false}
	 */
	public boolean addObject(final Object object);

	/**
	 * Remove the specified object from the collection.
	 * 
	 * @param object
	 *            the object to be removed
	 */
	public void removeObject(final Object object);

	/**
	 * Removes all objects from the collection.
	 */
	public void removeAll();

	/**
	 * Gets the object associated to the specified {@code values}.
	 * 
	 * @param values
	 *            the {@code values} to get the object for, those {@code values}
	 *            make up the key
	 * 
	 * @return the {@code Object} associated with the specified {@code values}
	 *         or {@code null} if no value is associated
	 */
	public Object getObject(final Object... values);

	/**
	 * Checks if the specified values are really supported by the collection,
	 * i.e. if those could be used to associate a object to the collection.
	 * 
	 * @param values
	 *            the values to be checked
	 * 
	 * @return {@code true} if those are supported, otherwise {@code false}
	 */
	public boolean supports(final Object... values);

	/**
	 * Get all the data within the collection.
	 * 
	 * @return all the data within the collection
	 */
	public Collection<?> getAll();

	/**
	 * Gets the amount of elements within the index.
	 * 
	 * @return the amount of elements within the index
	 */
	public int size();
}
