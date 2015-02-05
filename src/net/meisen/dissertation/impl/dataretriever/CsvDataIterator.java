package net.meisen.dissertation.impl.dataretriever;

import net.meisen.dissertation.model.dataretriever.DataIterator;
import net.meisen.dissertation.model.dataretriever.DataRecord;

/**
 * An iterator used to iterate over the csv-data of a {@code CsvDataCollection}.
 * 
 * @author pmeisen
 * 
 */
public class CsvDataIterator extends DataIterator<String> {
	private final CsvDataCollection coll;

	private String[] nextLine = null;

	/**
	 * Default constructor specifying the collection of the iterator.
	 * 
	 * @param coll
	 *            the collection the iterator is used for
	 */
	public CsvDataIterator(final CsvDataCollection coll) {
		this.coll = coll;
	}

	@Override
	public boolean hasNext() {
		if (nextLine == null) {
			nextLine = coll.readLine();
		}

		return nextLine != null;
	}

	@Override
	public DataRecord<String> next() {
		final String[] recordValues;
		if (this.nextLine == null) {
			recordValues = coll.readLine();
		} else {
			recordValues = nextLine;
			nextLine = null;
		}

		return new DataRecord<String>(coll, recordValues);
	}

}
