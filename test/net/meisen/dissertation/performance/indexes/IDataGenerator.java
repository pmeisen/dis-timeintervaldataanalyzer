package net.meisen.dissertation.performance.indexes;

/**
 * Helper interface to provide different data generators
 * 
 * @author pmeisen
 * 
 */
public interface IDataGenerator {

	/**
	 * Creates a generated {@code Object} the time of generation should be equal
	 * for each object.
	 * 
	 * @return a generated object
	 */
	public Object generateData();
}
