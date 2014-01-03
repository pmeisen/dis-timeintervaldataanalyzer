package net.meisen.dissertation.data.impl.dataretriever;

import java.sql.SQLException;

import net.meisen.dissertation.exceptions.DataRetrieverException;
import net.meisen.dissertation.models.impl.dataretriever.BaseDataRetriever;
import net.meisen.dissertation.models.impl.dataretriever.IDataRetrieverConfiguration;
import net.meisen.dissertation.models.impl.dataretriever.IQueryConfiguration;
import net.meisen.general.genmisc.types.Classes;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jolbox.bonecp.BoneCPDataSource;

public class DbDataRetriever extends BaseDataRetriever {
	private final static Logger LOG = LoggerFactory
			.getLogger(DbDataRetriever.class);
	private final BoneCPDataSource ds;

	public DbDataRetriever(final IDataRetrieverConfiguration config) {
		super(config);

		final DbConnectionConfig c = getConfig();

		// load the defined driver and make sure it exists
		final Class<?> driver = Classes.getClass(c.getDriver());
		if (driver == null) {
			exceptionRegistry.throwRuntimeException(
					DbDataRetrieverException.class, 1000, c.getDriver());
		}

		// get the DataSource and set the properties
		ds = new BoneCPDataSource();
		ds.setJdbcUrl(c.getUrl());
		ds.setUsername(c.getUsername());
		ds.setPassword(c.getPassword());

		// TODO add additional properties to the DbConnectionConfig
	}

	public void close() {
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
	protected Class<? extends IDataRetrieverConfiguration> supportedConfiguration() {
		return DbConnectionConfig.class;
	}

	@Override
	public DbDataCollection retrieve(
			final IQueryConfiguration queryConfiguration) {

		if (queryConfiguration == null) {
			exceptionRegistry.throwRuntimeException(
					DbDataRetrieverException.class, 1001);
		} else if (queryConfiguration instanceof DbQueryConfiguration) {
			// do nothing everything is fine
		} else {
			exceptionRegistry.throwRuntimeException(
					DataRetrieverException.class, 1002, DbDataRetriever.class
							.getName(),
					queryConfiguration.getClass().getName(),
					DbQueryConfiguration.class.getName());
		}

		final DbQueryConfiguration query = (DbQueryConfiguration) queryConfiguration;
		try {
			return new DbDataCollection(query, ds.getConnection());
		} catch (final SQLException e) {
			exceptionRegistry.throwRuntimeException(
					DbDataRetrieverException.class, 1002, e, ds.getJdbcUrl());

			// cannot happen
			return null;
		}
	}
}
