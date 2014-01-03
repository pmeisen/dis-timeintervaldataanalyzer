package net.meisen.dissertation.data.impl.dataretriever;

import net.meisen.dissertation.models.impl.dataretriever.IQueryConfiguration;

public class DbQueryConfiguration implements IQueryConfiguration {

	private String sqlQuery;

	public String getSqlQuery() {
		return sqlQuery;
	}

	public void setSqlQuery(final String sqlQuery) {
		this.sqlQuery = sqlQuery;
	}

}
