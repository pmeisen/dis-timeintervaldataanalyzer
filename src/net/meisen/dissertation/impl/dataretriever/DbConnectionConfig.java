package net.meisen.dissertation.impl.dataretriever;

import net.meisen.dissertation.model.dataretriever.IDataRetrieverConfig;

/**
 * The configuration of a database connection.
 * 
 * @author pmeisen
 * 
 */
public class DbConnectionConfig implements IDataRetrieverConfig {

	private String type;
	private String url;
	private String driver;
	private String username;
	private String password;

	/**
	 * Gets the type of the database, currently only {@code JDBC} is supported.
	 * 
	 * @return the type of the database
	 */
	public String getType() {
		return type;
	}

	/**
	 * Sets the type of the database.
	 * 
	 * @param type
	 *            the type of the database
	 */
	public void setType(final String type) {
		this.type = type;
	}

	/**
	 * Gets the {@code JDBC-url}.
	 * 
	 * @return the {@code JDBC-url}
	 */
	public String getUrl() {
		return url;
	}

	/**
	 * Sets the {@code JDBC-url}.
	 * 
	 * @param url
	 *            the {@code JDBC-url}
	 */
	public void setUrl(final String url) {
		this.url = url;
	}

	/**
	 * Gets the driver of the database.
	 * 
	 * @return the driver of the database
	 */
	public String getDriver() {
		return driver;
	}

	/**
	 * Sets the driver of the database to be used.
	 * 
	 * @param driver
	 *            the driver of the database
	 */
	public void setDriver(final String driver) {
		this.driver = driver;
	}

	/**
	 * Gets the user to be used to connect to the database.
	 * 
	 * @return the user to be used to connect to the database
	 */
	public String getUsername() {
		return username;
	}

	/**
	 * Sets the user to be used to connect to the database.
	 * 
	 * @param username
	 *            the user to be used to connect to the database
	 */
	public void setUsername(final String username) {
		this.username = username;
	}

	/**
	 * Get the password of the user used to connect to the database.
	 * 
	 * @return the password of the user used to connect to the database
	 */
	public String getPassword() {
		return password;
	}

	/**
	 * Sets the password of the user used to connect to the database.
	 * 
	 * @param password
	 *            the password of the user used to connect to the database
	 */
	public void setPassword(final String password) {
		this.password = password;
	}
}
