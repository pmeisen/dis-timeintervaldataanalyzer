package net.meisen.dissertation.impl.dataretriever;

import net.meisen.dissertation.model.dataretriever.IQueryConfiguration;

/**
 * The configuration of a query for a database.
 * 
 * @author pmeisen
 * 
 */
public class DbQueryConfig implements IQueryConfiguration {

	private String query;
	private String language;

	/**
	 * Gets the defined query.
	 * 
	 * @return the query
	 */
	public String getQuery() {
		return query;
	}

	/**
	 * Sets the query.
	 * 
	 * @param query
	 *            the query
	 */
	public void setQuery(final String query) {
		this.query = query;
	}

	/**
	 * Gets the language of the query (e.g. SQL).
	 * 
	 * @return the language of the query
	 */
	public String getLanguage() {
		return language;
	}

	/**
	 * Gets the language of the query (e.g. SQL).
	 * 
	 * @param language
	 *            the language of the query
	 */
	public void setLanguage(final String language) {
		this.language = language;
	}

	@Override
	public String toString() {
		return getLanguage() + ": " + getQuery();
	}
}
