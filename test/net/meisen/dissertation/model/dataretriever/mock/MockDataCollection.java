package net.meisen.dissertation.model.dataretriever.mock;

import net.meisen.dissertation.model.dataretriever.DataCollection;
import net.meisen.dissertation.model.dataretriever.DataIterator;
import net.meisen.dissertation.model.dataretriever.DataRecord;

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
	 * Mock of an {@code DataIterator}
	 * 
	 * @author pmeisen
	 * 
	 * @param <D>
	 *            the type of the names
	 */
	public static class MockDataIterator<D> extends DataIterator<D> {
		private final int amountOfData;
		private final MockDataCollection<D> col;
		private int counter = 0;

		/**
		 * Default constructor.
		 * 
		 * @param col
		 *            the {@code MockDataCollection}
		 * @param amountOfData
		 *            the amount of data to be iterated and generated
		 */
		public MockDataIterator(final MockDataCollection<D> col,
				final int amountOfData) {
			this.amountOfData = amountOfData;
			this.col = col;
		}

		@Override
		public boolean hasNext() {
			return counter < amountOfData;
		}

		@Override
		public DataRecord<D> next() {
			if (!hasNext()) {
				throw new IllegalStateException("Data exceeded!");
			}

			final Object[] data = new Object[col.getRecordSize()];
			for (int i = 0; i < col.getRecordSize(); i++) {
				data[i] = col.getNameOfPos(i) + " " + counter;
			}
			
			counter++;
			
			return new DataRecord<D>(col, data);
		}
	}

	private final int amountOfData;

	/**
	 * Call the default constructor.
	 * 
	 * @param names
	 *            the names
	 */
	public MockDataCollection(final D[] names) {
		this(names, 0);
	}

	/**
	 * Constructor to create a collection with random UUID data.
	 * 
	 * @param names
	 *            the names
	 * @param amountOfData
	 *            the amount of data to be generated
	 */
	public MockDataCollection(final D[] names, final int amountOfData) {
		super(names);

		this.amountOfData = amountOfData;
	}

	@Override
	public DataIterator<D> iterator() {
		return new MockDataIterator<D>(this, amountOfData);
	}

	@Override
	public void release() {
		// nothing to do
	}
}
