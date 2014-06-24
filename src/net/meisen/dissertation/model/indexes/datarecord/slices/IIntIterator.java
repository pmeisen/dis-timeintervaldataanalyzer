package net.meisen.dissertation.model.indexes.datarecord.slices;

/**
 * Iterator used to iterate over the integers of a bitmap.
 * 
 * @author pmeisen
 * 
 */
public interface IIntIterator {

	/**
	 * Is there more?
	 * 
	 * @return true, if there is more, false otherwise
	 */
	public boolean hasNext();

	/**
	 * Return the next integer
	 * 
	 * @return the integer
	 */
	public int next();

}