package net.meisen.dissertation.model.datasets;

import java.util.Iterator;

public interface IDataSet {

	public boolean hasNamedValue(final String name);
	
	public boolean isValidPosition(final int position);

	public Iterator<IDataRecord> iterate();
}
