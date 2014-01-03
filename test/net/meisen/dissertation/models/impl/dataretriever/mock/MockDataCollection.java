package net.meisen.dissertation.models.impl.dataretriever.mock;

import java.util.Iterator;

import net.meisen.dissertation.models.impl.dataretriever.DataCollection;
import net.meisen.dissertation.models.impl.dataretriever.DataRecord;

/**
 * Mock of a {@code DataCollection} for testing purposes
 * 
 * @author pmeisen
 * 
 * @param <D>
 *            the type of the names
 */
public class MockDataCollection<D> extends DataCollection<D> {

	/**
	 * Call the default constructor.
	 * 
	 * @param names
	 *            the names
	 */
	public MockDataCollection(final D[] names) {
		super(names);
	}

	@Override
	public Iterator<DataRecord<D>> open() {
		return null;
	}

	@Override
	public void close() {
		// nothing to do
	}
}
