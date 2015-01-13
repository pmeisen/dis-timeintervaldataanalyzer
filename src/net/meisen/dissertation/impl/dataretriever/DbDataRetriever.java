package net.meisen.dissertation.impl.dataretriever;

import java.sql.SQLException;

import net.meisen.dissertation.exceptions.DataRetrieverException;
import net.meisen.dissertation.model.dataretriever.BaseDataRetriever;
import net.meisen.dissertation.model.dataretriever.IConnectionPoolManager;
import net.meisen.dissertation.model.dataretriever.IDataRetrieverConfig;
import net.meisen.dissertation.model.dataretriever.IQueryConfiguration;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Classes;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * {@code DataRetriever} to retrieve data from a database.
 * 
 * @author pmeisen
 * 
 */
public class DbDataRetriever extends BaseDataRetriever {
	private final static Logger LOG = LoggerFactory
			.getLogger(DbDataRetriever.class);
	private final IConnectionPoolManager ds;

	/**
	 * Constructor with the {@code DataRetrieverConfiguration}, the
	 * {@code config} must be of a valid type.
	 * 
	 * @param id
	 *            an identifier for the retriever
	 * @param config
	 *            the {@code DataRetrieverConfiguration} used for the
	 *            {@code DataRetriever}
	 * 
	 * @see #supportedConfiguration()
	 */
	public DbDataRetriever(final String id, final IDataRetrieverConfig config) {
		super(id, config);

		final DbConnectionConfig c = getConfig();

		// load the defined driver and make sure it exists
		final Class<?> driver = Classes.getClass(c.getDriver());
		if (driver == null) {
			throw new ForwardedRuntimeException(
					DbDataRetrieverException.class, 1000, c.getDriver());
		}

		// get the DataSource and set the properties
		ds = new HikariConnectionPoolManager();
		ds.setJdbcUrl(c.getUrl());
		ds.setUsername(c.getUsername());
		ds.setPassword(c.getPassword());
	}

	@Override
	public void release() {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Closing the DbDataRetriever for database '"
					+ ds.getJdbcUrl() + "'");
		}
		ds.close();
	}

	@Override
	protected boolean needConfiguration() {
		return true;
	}

	@Override
	protected Class<? extends IDataRetrieverConfig> supportedConfiguration() {
		return DbConnectionConfig.class;
	}

	@Override
	public DbDataCollection retrieve(
			final IQueryConfiguration queryConfiguration) {

		if (queryConfiguration == null) {
			exceptionRegistry.throwRuntimeException(
					DbDataRetrieverException.class, 1001);
		} else if (queryConfiguration instanceof DbQueryConfig) {
			// do nothing everything is fine
		} else {
			exceptionRegistry.throwRuntimeException(
					DataRetrieverException.class, 1002, DbDataRetriever.class
							.getName(),
					queryConfiguration.getClass().getName(),
					DbQueryConfig.class.getName());
		}

		final DbQueryConfig query = (DbQueryConfig) queryConfiguration;
		try {
			return new DbDataCollection(query, ds.getConnection());
		} catch (final SQLException e) {
			exceptionRegistry.throwRuntimeException(
					DbDataRetrieverException.class, 1002, e, ds.getJdbcUrl());

			// cannot happen
			return null;
		} catch (final ForwardedRuntimeException e) {
			exceptionRegistry.throwRuntimeException(e);

			// cannot happen
			return null;
		}
	}
}
