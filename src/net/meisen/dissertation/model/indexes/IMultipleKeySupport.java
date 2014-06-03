package net.meisen.dissertation.model.indexes;

/**
 * This interface describes the property of an {@code BaseIndexedCollection},
 * which identifies that multiple {@code IndexKeyDefinition} instances can be
 * defined. The {@code BaseIndexedCollection} defines based on the passed values
 * which internal structure should be used.
 * 
 * @author pmeisen
 * 
 */
public interface IMultipleKeySupport extends IIndexedCollection {

	/**
	 * Uses a specific {@code IndexKeyDefinition} to get the object for a
	 * specified key.
	 * 
	 * @param keyDefNr
	 *            the number of the {@code IndexKeyDefinition} to be used
	 * @param values
	 *            the values to retrieve the object for
	 * 
	 * @return the {@code Object} associated to the specified keys
	 * 
	 * @throws IllegalArgumentException
	 *             if the key doesn't fit
	 * 
	 * @see IndexKeyDefinition
	 */
	public Object getObjectByDefNr(final int keyDefNr, final Object... values);
}
