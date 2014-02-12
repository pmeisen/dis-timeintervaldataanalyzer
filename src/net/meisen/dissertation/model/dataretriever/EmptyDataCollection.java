package net.meisen.dissertation.model.dataretriever;

import java.util.ArrayList;

/**
 * An empty {@code DataCollection} which has no values and names.
 * 
 * @author pmeisen
 * 
 * @param <D>
 *            the type of the names
 */
public class EmptyDataCollection<D> extends DataCollection<D> {

	/**
	 * Default constructor which sets the names to an empty {@code List}.
	 */
	public EmptyDataCollection() {
		super();
		setNames(new ArrayList<D>());
	}

	@Override
	public DataIterator<D> iterator() {
		return new DataIterator<D>() {

			@Override
			public boolean hasNext() {
				return false;
			}

			@Override
			public DataRecord<D> next() {
				throw new IllegalStateException("No next value available");
			}
		};
	}

	@Override
	public void release() {
		// nothing to do
	}
}
