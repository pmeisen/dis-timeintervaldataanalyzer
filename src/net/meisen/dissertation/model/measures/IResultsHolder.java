package net.meisen.dissertation.model.measures;

/**
 * A holder of intermediate results (i.e. facts calculated), which are not
 * connected to any record.
 * 
 * @author pmeisen
 * 
 */
public interface IResultsHolder extends IDoubleHolder {

	/**
	 * Adds the specified result.
	 * 
	 * @param result
	 *            the result to be added
	 */
	public void add(final double result);
}
