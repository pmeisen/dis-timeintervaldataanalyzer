package net.meisen.dissertation.model.indexes;

/**
 * A resolver used to resolve the identifier for a specific object. Such an
 * {@code IdResolver} can be used when defining a {@code IndexKeyDefinition}.
 * 
 * @author pmeisen
 * @param <T>
 *            the type of the identifier
 * 
 */
public interface IndexedCollectionIdResolver<T extends Object> {

	/**
	 * Determines the identifier for the specified object.
	 * 
	 * @param object
	 *            the object to get the identifier for
	 * 
	 * @return the identifier for the specified object
	 */
	public T getId(final Object object);

	/**
	 * Gets the identifiers type.
	 * 
	 * @return the type of the identifier
	 */
	public Class<T> getIdType();
}
