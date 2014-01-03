package net.meisen.dissertation.data.impl.dataretriever;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Iterator;

import net.meisen.dissertation.models.impl.dataretriever.DataCollection;
import net.meisen.dissertation.models.impl.dataretriever.DataRecord;

public class DbDataCollection extends DataCollection<String> {
	private final Connection connection;
	private final DbQueryConfiguration query;

	public DbDataCollection(final DbQueryConfiguration query,
			final Connection connection) {
		super(getColumnNames(query, connection));

		// keep some needed stuff
		this.connection = connection;
		this.query = query;
	}

	protected static String[] getColumnNames(final DbQueryConfiguration query,
			final Connection connection) {

		try {
			// create the statement
			final Statement stmnt = connection.createStatement(
					ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
			final ResultSet rs = stmnt.executeQuery("SELECT * FROM ("
					+ query.getSqlQuery() + ") WHERE 1=0");
			final ResultSetMetaData metaData = rs.getMetaData();

			// create the String-Array
			final int size = metaData.getColumnCount();
			final String[] columns = new String[size];
			for (int i = 1; i <= size; i++) {
				columns[i - 1] = metaData.getColumnName(i);
			}

			// close the statement
			stmnt.close();

			return columns;
		} catch (final SQLException e) {
			throw new IllegalStateException();
		}
	}

	@Override
	public Iterator<DataRecord<String>> open() {
		try {
			final Statement stmnt = connection.createStatement(
					ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
			final ResultSet rs = stmnt.executeQuery(query.getSqlQuery());

			return null;
		} catch (final SQLException e) {
			throw new IllegalStateException();
		}
	}

	@Override
	public void close() {
		try {
			connection.close();
		} catch (final SQLException e) {

		}
	}
}
