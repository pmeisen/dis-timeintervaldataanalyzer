package net.meisen.dissertation.model.datasets;

import net.meisen.dissertation.model.dataretriever.DataRecord;

public class DataRetrieverDataSetRecord implements IDataRecord {
	private final DataRecord<String> record;

	public DataRetrieverDataSetRecord(final DataRecord<String> record) {
		this.record = record;
	}

	@Override
	public Object getValue(final int position) {
		return record.getDataByPos(position);
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
}
