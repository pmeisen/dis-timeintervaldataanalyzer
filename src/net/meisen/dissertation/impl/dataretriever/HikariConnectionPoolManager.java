package net.meisen.dissertation.impl.dataretriever;

import java.sql.Connection;
import java.sql.SQLException;

import com.zaxxer.hikari.HikariDataSource;

import net.meisen.dissertation.model.dataretriever.IConnectionPoolManager;

/**
 * A {@code Hikari} based connection-pool manager.
 * 
 * @author pmeisen
 * 
 * @see HikariDataSource
 * 
 */
public class HikariConnectionPoolManager implements IConnectionPoolManager {
	private final HikariDataSource ds;

	/**
	 * Default constructor.
	 */
	public HikariConnectionPoolManager() {
		ds = new HikariDataSource();
		ds.setReadOnly(true);
	}

	@Override
	public String getJdbcUrl() {
		return ds.getJdbcUrl();
	}

	@Override
	public void close() {
		ds.close();
	}

	@Override
	public Connection getConnection() throws SQLException {
		try {
			return ds.getConnection();
		} catch (final RuntimeException e) {
			throw new SQLException(e.getMessage(), e);
		}
	}

	@Override
	public void setJdbcUrl(final String jdbcUrl) {
		ds.setJdbcUrl(jdbcUrl);
	}

	@Override
	public void setUsername(final String username) {
		ds.setUsername(username);
	}

	@Override
	public void setPassword(final String password) {
		ds.setPassword(password);
	}

	@Override
	public void setProperty(final String property, final Object value) {
		ds.addDataSourceProperty(property, value);
	}
}
