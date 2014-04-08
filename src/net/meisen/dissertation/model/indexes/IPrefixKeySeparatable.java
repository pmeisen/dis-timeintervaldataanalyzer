package net.meisen.dissertation.model.indexes;

import java.util.List;


/**
 * This interface describes the property of an {@code BaseIndexedCollection}, which
 * identifies that a key can be separated to query for data. <br/>
 * <br/>
 * <b>Example:</b><br/>
 * A key is defined to consists of three values (in the specified order):
 * <ul>
 * <li>getName</li>
 * <li>getFirstName</li>
 * <li>getAge</li>
 * </ul>
 * It is possible to query for data by just specifying just a prefix of the key
 * e.g. {@code name} of a key or the {@code name} and the {@code firstName}.
 * 
 * @author pmeisen
 * 
 */
public interface IPrefixKeySeparatable extends IIndexedCollection {

	/**
	 * Get all the objects with the specified prefix of {@code keys}. The key
	 * can be partly, i.e. only the first keys to get more than one object.
	 * 
	 * @param keys
	 *            the keys to resolve the object or the
	 *            {@code BaseIndexedCollection}
	 * 
	 * @return all the objects, if the key is a <i>full</i> key, i.e. one that
	 *         resolves down to the leaves, a {@code List} with exactly one item
	 *         or the empty list is returned
	 */
	public List<Object> getObjects(final Object... keys);

	/**
	 * Gets the amount of elements within the index. Using a
	 * {@code IPrefixKeySeparatable} it might be considerable to get all
	 * elements {@link #getAll()} and count those if all elements are needed.
	 * 
	 * @return the amount of elements within the index
	 */
	public int size();
}
