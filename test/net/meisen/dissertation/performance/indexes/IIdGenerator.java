package net.meisen.dissertation.performance.indexes;

/**
 * Helper interface to provide different data generators
 * 
 * @author pmeisen
 * 
 */
public interface IIdGenerator {

	/**
	 * Creates a generated {@code Object} the time of generation should be equal
	 * for each object.
	 * 
	 * @param data
	 *            the data object to create the id for
	 * 
	 * @return a generated object
	 */
	public Object generateId(final Object data);
}
