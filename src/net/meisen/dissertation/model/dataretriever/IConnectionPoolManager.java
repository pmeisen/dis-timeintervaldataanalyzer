package net.meisen.dissertation.model.dataretriever;

import java.sql.Connection;
import java.sql.SQLException;

/**
 * Interface for a manager of database connections.
 * 
 * @author pmeisen
 * 
 */
public interface IConnectionPoolManager {

	/**
	 * Gets the used jdbc-url.
	 * 
	 * @return the used jdbc-url
	 */
	public String getJdbcUrl();

	/**
	 * Closes the connection pool.
	 */
	public void close();

	/**
	 * Gets a new connection from the pool.
	 * 
	 * @return the new connection
	 * 
	 * @throws SQLException
	 *             if the connection cannot be provided
	 */
	public Connection getConnection() throws SQLException;

	/**
	 * Sets the jdbc-url to be used.
	 * 
	 * @param url
	 *            the jdbc-url to be used
	 */
	public void setJdbcUrl(final String url);

	/**
	 * Sets the user's name.
	 * 
	 * @param username
	 *            the user's name
	 */
	public void setUsername(final String username);

	/**
	 * Sets the password.
	 * 
	 * @param password
	 *            the password
	 */
	public void setPassword(final String password);

	/**
	 * Sets additional properties.
	 * 
	 * @param property
	 *            the property to be set
	 * @param value
	 *            the value to be set
	 */
	public void setProperty(final String property, final Object value);
}
