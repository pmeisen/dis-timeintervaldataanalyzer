package net.meisen.dissertation.data.impl.dataretriever;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;

import net.meisen.dissertation.models.impl.dataretriever.DataCollection;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * A {@code DbDataCollection} is a database's data collection, i.e. a collection
 * which contains the data retrieved from a database using a {@code query}. The
 * collection does not cache any data, i.e. a new iterator (pointing to the
 * database's data) is opened for every call to {@link #iterate()}. Furthermore
 * every call to {@link #get()} reloads the data from the database.
 * 
 * @author pmeisen
 * 
 */
public class DbDataCollection extends DataCollection<String> {
	private final Connection connection;
	private final DbQueryConfig query;

	/**
	 * 
	 * 
	 * @param query
	 * @param connection
	 */
	public DbDataCollection(final DbQueryConfig query,
			final Connection connection) {
		super(getColumnNames(query, connection));

		// keep some needed stuff
		this.connection = connection;
		this.query = query;
	}

	/**
	 * Gets the column names for the specified {@code query}.
	 * 
	 * @param query
	 *            the {@code DbQueryConfig} to get the columns names from
	 * @param connection
	 *            the {@code Connection} to fire the query against
	 * 
	 * @return the names of the columns of the query
	 */
	protected static String[] getColumnNames(final DbQueryConfig query,
			final Connection connection) {

		if (!"sql".equalsIgnoreCase(query.getLanguage())) {
			final ForwardedRuntimeException exForwarded = new ForwardedRuntimeException(
					DbDataRetrieverException.class, 1009, query.getLanguage());

			closeConnection(connection, false);
			throw exForwarded;
		}

		try {
			// create the statement
			final Statement stmnt = connection.createStatement(
					ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
			final ResultSet rs = stmnt.executeQuery("SELECT * FROM ("
					+ query.getQuery() + ") WHERE 1=0");
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
			final ForwardedRuntimeException exForwarded = new ForwardedRuntimeException(
					DbDataRetrieverException.class, 1005, e, query.getQuery());

			closeConnection(connection, false);
			throw exForwarded;
		}
	}

	/**
	 * Closes the specified {@code Connection} and exception is thrown if
	 * {@code throwException} is {@code true}.
	 * 
	 * @param connection
	 *            the {@code Connection} to be closed
	 * @param throwException
	 *            defines if the connection should be thrown {@code true} or not
	 */
	protected static void closeConnection(final Connection connection,
			final boolean throwException) {
		if (connection == null) {
			return;
		}

		try {
			// we cannot use the connection so close it
			connection.close();
		} catch (final SQLException e) {
			if (throwException) {
				final ForwardedRuntimeException exForwarded = new ForwardedRuntimeException(
						DbDataRetrieverException.class, 1004, e);
				throw exForwarded;
			}
		}
	}

	@Override
	public DbDataIterator iterate() {
		try {
			final Statement stmnt = connection.createStatement(
					ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
			final ResultSet rs = stmnt.executeQuery(query.getQuery());

			return new DbDataIterator(this, rs);
		} catch (final SQLException e) {
			final ForwardedRuntimeException exForwarded = new ForwardedRuntimeException(
					DbDataRetrieverException.class, 1003, e, query.getQuery());
			throw exForwarded;
		}
	}

	@Override
	public void release() {
		closeConnection(connection, true);
	}

	/**
	 * Get the defined query for the collection.
	 * 
	 * @return the query for the collection
	 */
	public String getQuery() {
		return query.getQuery();
	}
}
