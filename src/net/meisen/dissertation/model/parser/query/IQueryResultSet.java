package net.meisen.dissertation.model.parser.query;

public interface IQueryResultSet extends IQueryResult, Iterable<Object[]> {

	public Class<?>[] getTypes();

	public String[] getNames();
}
