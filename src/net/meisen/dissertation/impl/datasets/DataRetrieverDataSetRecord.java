package net.meisen.dissertation.impl.datasets;

import net.meisen.dissertation.model.dataretriever.BaseDataRetriever;
import net.meisen.dissertation.model.dataretriever.DataRecord;
import net.meisen.dissertation.model.datasets.IDataRecord;

/**
 * A {@code DataRecord} which wraps a {@link DataRecord} from a
 * {@code DataRetriever}.
 * 
 * @author pmeisen
 * 
 * @see BaseDataRetriever
 * @see DataRecord
 * 
 */
public class DataRetrieverDataSetRecord implements IDataRecord {
	private final DataRecord<String> record;

	/**
	 * The constructor specifies which {@code DataRecord} should be wrapped by
	 * {@code this}.
	 * 
	 * @param record
	 *            the {@code DataRecord} to be wrapped
	 */
	public DataRetrieverDataSetRecord(final DataRecord<String> record) {
		this.record = record;
	}

	@Override
	public Object getValue(final int position) {
		return record.getDataByPos(position - 1);
	}

	@Override
	public Object getValue(final String name) {
		return record.getData(name);
	}

	@Override
	public boolean hasNamedValue(final String name) {
		return record.getNames().contains(name);
	}

	@Override
	public int getSize() {
		return record.getSize();
	}

	@Override
	public String toString() {
		return record == null ? null : record.toString();
	}

	@Override
	public boolean isValidPosition(int position) {
		return position > 0 && position <= record.getSize();
	}
}
