package net.meisen.dissertation.impl.dataretriever;

import java.sql.ResultSet;
import java.sql.SQLException;

import net.meisen.dissertation.model.dataretriever.DataIterator;
import net.meisen.dissertation.model.dataretriever.DataRecord;
import net.meisen.dissertation.model.dataretriever.ICloseableDataIterator;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Iterator used to iterate through the retrieved data from a database.
 * 
 * @author pmeisen
 * 
 */
public class DbDataIterator extends DataIterator<String> implements
		ICloseableDataIterator {
	private final static Logger LOG = LoggerFactory
			.getLogger(DbDataIterator.class);

	private final ResultSet rs;
	private final DbDataCollection collection;

	Boolean lastNext = null;

	/**
	 * The constructor of a {@code DbDataIterator}.
	 * 
	 * @param collection
	 *            the {@code DbDataCollection} this iterator is defined for
	 * @param rs
	 *            the {@code ResultSet} this {@code DbDataIterator} is based on
	 */
	public DbDataIterator(final DbDataCollection collection, final ResultSet rs) {
		this.collection = collection;
		this.rs = rs;
	}

	@Override
	public boolean hasNext() {
		try {
			final boolean next = lastNext == null ? rs.next() : lastNext;
			lastNext = next;
			if (next) {
				return true;
			} else {
				close();
				return false;
			}
		} catch (final SQLException e) {
			final ForwardedRuntimeException exForwarded = new ForwardedRuntimeException(
					DbDataRetrieverException.class, 1006, e,
					collection.getQuery());
			throw exForwarded;
		}
	}

	/**
	 * Closes the {@code DbDataIterator} so that it cannot be used anymore. All
	 * calls to {@link #hasNext()} or {@link #next()} will cause an exception.
	 */
	@Override
	public void close() {

		try {
			if (rs.isClosed()) {
				return;
			} else {
				rs.close();
			}

			// log the closing
			if (LOG.isTraceEnabled()) {
				LOG.trace("Closed resultSet of query '" + collection.getQuery()
						+ "'");
			}
		} catch (final SQLException e) {
			final ForwardedRuntimeException exForwarded = new ForwardedRuntimeException(
					DbDataRetrieverException.class, 1007, e,
					collection.getQuery());
			throw exForwarded;
		}
	}

	/**
	 * Just to make sure that the {@code DbDataIterator} will be closed one day,
	 * we do it here.
	 */
	@Override
	public void finalize() {
		close();
	}

	@Override
	public DataRecord<String> next() {
		final DataRecord<String> record = new DataRecord<String>(collection);

		if (lastNext == null) {
			try {
				rs.next();
			} catch (final SQLException e) {
				final ForwardedRuntimeException exForwarded = new ForwardedRuntimeException(
						DbDataRetrieverException.class, 1006, e,
						collection.getQuery());
				throw exForwarded;
			}
		} else {
			lastNext = null;
		}

		for (final String colName : collection.getNames()) {
			try {
				record.setData(colName, rs.getObject(colName));
			} catch (final SQLException e) {
				final ForwardedRuntimeException exForwarded = new ForwardedRuntimeException(
						DbDataRetrieverException.class, 1008, e, colName,
						collection.getQuery());
				throw exForwarded;
			}
		}

		return record;
	}
}
