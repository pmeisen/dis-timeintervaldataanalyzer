package net.meisen.dissertation.impl.dataretriever;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;

import net.meisen.dissertation.model.dataretriever.DataCollection;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A {@code DbDataCollection} is a database's data collection, i.e. a collection
 * which contains the data retrieved from a database using a {@code query}. The
 * collection does not cache any data, i.e. a new iterator (pointing to the
 * database's data) is opened for every call to {@link #iterator()}. Furthermore
 * every call to {@link #get()} reloads the data from the database.
 * 
 * @author pmeisen
 * 
 */
public class DbDataCollection extends DataCollection<String> {
	private final static Logger LOG = LoggerFactory
			.getLogger(DbDataCollection.class);

	private final Connection connection;
	private final DbQueryConfig query;
	private final PreparedStatement statement;

	/**
	 * Constructor to create a {@code DataCollection} for the specified
	 * {@code query} on the specified {@code connection}. The {@code connection}
	 * will be closed whenever the {@code DataCollection} is released (i.e.
	 * {@link #release()}.
	 * 
	 * @param query
	 *            the {@code DbQueryConfig} to fire against the database to
	 *            retrieve the data for the collection
	 * @param connection
	 *            the {@code Connection} to be used to retrieve the data
	 */
	public DbDataCollection(final DbQueryConfig query,
			final Connection connection) {

		// set the attributes
		this.connection = connection;
		this.query = query;

		// check the language (currently we only support SQL)
		if (!"sql".equalsIgnoreCase(query.getLanguage())) {
			final ForwardedRuntimeException exForwarded = new ForwardedRuntimeException(
					DbDataRetrieverException.class, 1009, query.getLanguage());

			closeConnection(false);
			throw exForwarded;
		}

		// prepare a statement for the query
		try {
			this.statement = connection.prepareStatement(query.getQuery(),
					ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
		} catch (final SQLException e) {
			final ForwardedRuntimeException exForwarded = new ForwardedRuntimeException(
					DbDataRetrieverException.class, 1011, e, query.getQuery());

			closeConnection(false);
			throw exForwarded;
		}

		// finally make sure the names are set
		setNames(getColumnNames());
	}

	/**
	 * Gets the column names for the specified {@code query}.
	 * 
	 * @return the names of the columns of the query
	 */
	protected String[] getColumnNames() {

		try {
			final ResultSetMetaData metaData = statement.getMetaData();

			// create the String-Array
			final int size = metaData.getColumnCount();
			final String[] columns = new String[size];
			for (int i = 1; i <= size; i++) {
				columns[i - 1] = metaData.getColumnName(i);
			}

			return columns;
		} catch (final SQLException e) {
			final ForwardedRuntimeException exForwarded = new ForwardedRuntimeException(
					DbDataRetrieverException.class, 1005, e, query.getQuery());

			throw exForwarded;
		}
	}

	/**
	 * Closes the specified {@code Connection} and exception is thrown if
	 * {@code throwException} is {@code true}.
	 * 
	 * @param throwException
	 *            defines if the connection should be thrown {@code true} or not
	 */
	protected void closeConnection(final boolean throwException) {

		if (statement != null) {
			try {
				statement.close();
			} catch (final SQLException e) {
				if (throwException) {
					final ForwardedRuntimeException exForwarded = new ForwardedRuntimeException(
							DbDataRetrieverException.class, 1010, e,
							query.getQuery());
					throw exForwarded;
				} else {
					LOG.error("Unable to close the statement for query '"
							+ query.getQuery() + "'.", e);
				}
			}
		}

		if (connection != null) {
			try {
				// we cannot use the connection so close it
				connection.close();
			} catch (final SQLException e) {
				if (throwException) {
					final ForwardedRuntimeException exForwarded = new ForwardedRuntimeException(
							DbDataRetrieverException.class, 1004, e);
					throw exForwarded;
				} else {
					LOG.error("Unable to close the connection for query '"
							+ query.getQuery() + "'.", e);
				}
			}
		}

		if (LOG.isTraceEnabled()) {
			LOG.trace("Closed connection and preparedStatement of DataCollection (query: '"
					+ query.getQuery() + "').");
		}
	}

	@Override
	public DbDataIterator iterator() {
		try {
			final ResultSet rs = statement.executeQuery();
			return new DbDataIterator(this, rs);
		} catch (final SQLException e) {
			final ForwardedRuntimeException exForwarded = new ForwardedRuntimeException(
					DbDataRetrieverException.class, 1003, e, getQuery());
			throw exForwarded;
		}
	}

	@Override
	public void release() {
		closeConnection(true);
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
