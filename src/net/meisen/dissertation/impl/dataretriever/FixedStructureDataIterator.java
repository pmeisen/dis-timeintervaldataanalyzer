package net.meisen.dissertation.impl.dataretriever;

import java.util.List;
import java.util.NoSuchElementException;

import net.meisen.dissertation.model.dataretriever.DataIterator;
import net.meisen.dissertation.model.dataretriever.DataRecord;

/**
 * A {@code FixedStructureDataIterator} is used to iterate through the data of a
 * {@code FixedStructureDataCollection}.
 * 
 * @author pmeisen
 * 
 */
public class FixedStructureDataIterator extends DataIterator<String> {

	private final FixedStructureDataCollection collection;

	private int current;

	/**
	 * An {@code iterator} used to iterate through the {@code collection}.
	 * 
	 * @param collection
	 *            the {@code FixedStructureDataCollection} to iterate through
	 */
	public FixedStructureDataIterator(
			final FixedStructureDataCollection collection) {
		this.collection = collection;
		this.current = 0;
	}

	@Override
	public boolean hasNext() {
		return current < collection.size();
	}

	@Override
	public DataRecord<String> next() {
		if (!hasNext()) {
			throw new NoSuchElementException(
					"No further elements available (size: '"
							+ collection.size() + "').");
		}

		// get the data
		final List<FixedStructureDataRetrieverConfigEntry> entries = collection
				.getEntries();
		final int size = entries.size();
		final Object[] data = new Object[size];
		for (int i = 0; i < size; i++) {
			final FixedStructureDataRetrieverConfigEntry entry = entries.get(i);
			data[i] = entry.createValue();
		}

		// increase to the next one
		current++;

		// create a new record
		return new DataRecord<String>(collection, data);
	}
}
