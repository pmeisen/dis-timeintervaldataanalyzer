package net.meisen.dissertation.model.parser.query;

public interface IQueryResultSet extends IQueryResult {

	public Class<?>[] getTypes();
	
	public String[] getNames();

}
