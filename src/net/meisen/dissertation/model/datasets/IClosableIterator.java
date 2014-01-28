package net.meisen.dissertation.model.datasets;

import java.util.Iterator;

/**
 * Marks a {@code Iterator} to be closeable, i.e. that it should be closed
 * (calling the method {@link #close()}) if it isn't used anymore.
 * 
 * @author pmeisen
 * 
 * @param <D>
 *            the type of the data which is iterated
 * 
 */
public interface IClosableIterator<D> extends Iterator<D> {

	/**
	 * Closes the iterator, i.e. is not used anymore, so that resources used by
	 * the iterator can be freed.
	 */
	public void close();
}
