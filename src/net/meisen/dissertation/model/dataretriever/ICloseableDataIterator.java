package net.meisen.dissertation.model.dataretriever;

/**
 * Marks a {@code DataIterator} to be closeable, i.e. that it should be closed
 * (calling the method {@link #close()}) if it isn't used anymore.
 * 
 * @author pmeisen
 * 
 */
public interface ICloseableDataIterator {

	/**
	 * Closes the iterator, i.e. is not used anymore, so that resources used by
	 * the iterator can be freed.
	 */
	public void close();
}
