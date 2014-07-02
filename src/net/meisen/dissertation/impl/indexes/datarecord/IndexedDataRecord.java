package net.meisen.dissertation.impl.indexes.datarecord;

import net.meisen.dissertation.model.datasets.IDataRecord;

/**
 * A {@code DataRecord} used for indexed and cached records.
 * 
 * @author pmeisen
 * 
 */
public class IndexedDataRecord implements IDataRecord {

	private final IndexedDataRecordMeta meta;
	private final Object[] values;

	/**
	 * Constructor specifying the meta-information and the values of the record.
	 * 
	 * @param meta
	 *            the meta-information
	 * @param values
	 *            the values
	 */
	public IndexedDataRecord(final IndexedDataRecordMeta meta,
			final Object[] values) {
		this.meta = meta;
		this.values = values;
	}

	@Override
	public boolean hasNamedValue(final String name) {
		return meta.getPosition(name, true) != -1;
	}

	@Override
	public Object getValue(final int position) throws RuntimeException {
		if (!isValidPosition(position)) {
			throw new IllegalArgumentException("The position '" + position
					+ "' is invalid.");
		}

		return values[position];
	}

	@Override
	public Object getValue(final String name) throws RuntimeException {
		final int pos = meta.getPosition(name, true);
		if (!isValidPosition(pos)) {
			throw new IllegalArgumentException("The name '" + name
					+ "' cannot be found wthin the record.");
		}

		return getValue(pos);
	}

	@Override
	public boolean isValidPosition(final int position) {
		return position > -1 && position < meta.getTypes().length;
	}

	@Override
	public int getSize() {
		return meta.getTypes().length;
	}
}
