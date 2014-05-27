package net.meisen.dissertation.model.dataretriever;

import java.util.Iterator;

/**
 * Base Implementation of a {@code Iterator} used to iterate over the results
 * retrieved from a database. The base implementation is just used to finalize
 * the implementation of the {@link #remove()} method, which is not supported by
 * a {@code DataIterator} but declared for a {@code Iterator}.
 * 
 * @author pmeisen
 * 
 * @param <D>
 *            the type of the fields of the {@code DataRecord} of the
 *            {@code DataIterator}
 */
public abstract class DataIterator<D> implements Iterator<DataRecord<D>> {

	@Override
	public final void remove() {
		throw new UnsupportedOperationException(
				"A DataIterator does not support the remove operation.");
	}
}
