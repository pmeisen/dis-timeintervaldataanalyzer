package net.meisen.dissertation.model.datasets;

public interface IDataRecord {

	public boolean hasNamedValue(final String name);

	public Object getValue(final int position);

	public Object getValue(final String name);

	public int getSize();

}
