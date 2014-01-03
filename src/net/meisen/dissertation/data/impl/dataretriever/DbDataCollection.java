package net.meisen.dissertation.data.impl.dataretriever;

import java.util.Iterator;

import net.meisen.dissertation.models.impl.dataretriever.DataCollection;
import net.meisen.dissertation.models.impl.dataretriever.DataRecord;

public class DbDataCollection extends DataCollection<String> {

	public DbDataCollection(final String[] names) {
		super(names);
	}
	
//	private String[] getColumnNames() {
//		
//	}
	
	@Override
	public Iterator<DataRecord<String>> open() {
		return null;
	}

	@Override
	public void close() {
		
	}

}
