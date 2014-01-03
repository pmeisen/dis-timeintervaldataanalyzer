package net.meisen.dissertation.data.impl.dataretriever;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.jolbox.bonecp.BoneCPDataSource;

import net.meisen.dissertation.models.impl.dataretriever.BaseDataRetriever;
import net.meisen.dissertation.models.impl.dataretriever.DataCollection;
import net.meisen.dissertation.models.impl.dataretriever.DataRecord;
import net.meisen.dissertation.models.impl.dataretriever.IDataRetrieverConfiguration;
import net.meisen.dissertation.models.impl.dataretriever.IQueryConfiguration;
import net.meisen.general.genmisc.types.Classes;

public class DbDataRetriever extends BaseDataRetriever {

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
		// TODO ds muss irgendwie wieder mit ds.close() geschlossen werden
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
	public DataCollection<?> retrieve(
			final IQueryConfiguration queryConfiguration) {
		final List<?> res = new ArrayList<Object>();

		if (queryConfiguration == null) {
			exceptionRegistry.throwRuntimeException(
					DbDataRetrieverException.class, 1001);
		} else if (queryConfiguration instanceof DbQueryConfiguration) {
			// do nothing everything is fine
		} else {
			throw new IllegalArgumentException("The '"
					+ DbDataRetriever.class.getName()
					+ "' does not support a queryConfiguration of type '"
					+ queryConfiguration.getClass().getName() + "'");
		}
		
		final DbQueryConfiguration query = (DbQueryConfiguration) queryConfiguration;
		

		return new DataCollection<String>(new String[] { "A" }) {

			@Override
			public Iterator<DataRecord<String>> open() {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			public void close() {
				// TODO Auto-generated method stub

			}
		};
	}

	public List<Map<String, Object>> query(final String dbName,
			final String query) throws SQLException {
		final Connection c = DriverManager.getConnection(
				"jdbc:hsqldb:hsql://localhost:6666/" + dbName, "SA", "");
		final Statement stmnt = c.createStatement();
		final ResultSet rs = stmnt.executeQuery(query);
		final ResultSetMetaData metaData = rs.getMetaData();

		final List<Map<String, Object>> table = new ArrayList<Map<String, Object>>();
		while (rs.next()) {
			final Map<String, Object> row = new LinkedHashMap<String, Object>();

			for (int i = 1; i <= metaData.getColumnCount(); i++) {
				final String key = metaData.getColumnName(i);
				final Object value = rs.getObject(key);

				row.put(key, value);
			}

			table.add(row);
		}

		// close everything
		rs.close();
		stmnt.close();
		c.close();

		return table;
	}
}
