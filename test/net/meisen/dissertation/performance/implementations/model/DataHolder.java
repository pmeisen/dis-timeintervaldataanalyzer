package net.meisen.dissertation.performance.implementations.model;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import net.meisen.dissertation.help.Db;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.general.genmisc.types.Dates;

/**
 * Helper class to load data from the database.
 * 
 * @author pmeisen
 * 
 */
public class DataHolder {
	private final Db db;
	private final List<Map<String, Object>> records;

	/**
	 * Constructs the holder for the specified model.
	 * 
	 * @param model
	 *            the model to create the holder, needed to remove invalid data
	 *            (invalid considering the UTC and the other implementations)
	 */
	public DataHolder(final TidaModel model) {
		this(model,
				"/net/meisen/dissertation/performance/implementations/model/ghdataHsql.zip");
	}

	/**
	 * Constructs the holder for the specified model.
	 * 
	 * @param model
	 *            the model to create the holder, needed to remove invalid data
	 *            (invalid considering the UTC and the other implementations)
	 * @param dbPath
	 *            the path to the database to be used; must be valid according
	 *            to the test-scenario
	 */
	public DataHolder(final TidaModel model, final String dbPath) {

		// open the database
		db = new Db();
		try {
			db.addDb("tida", dbPath);
			db.setUpDb();
		} catch (final IOException e) {
			db.shutDownDb();
			throw new IllegalStateException("Database cannot be loaded.", e);
		}

		// query the database
		final List<Map<String, Object>> records;
		final String query = "SELECT KEY, PERSON, TASKTYPE, WORKAREA, INTERVAL_START, INTERVAL_END FROM SMC_DATA ORDER BY RAND()";
		try {
			records = db.query("tida", query);
		} catch (final SQLException e) {
			db.shutDownDb();
			throw new IllegalStateException("Could not execute the query.", e);
		}

		// filter some invalid records, which do not perform correctly
		final BaseMapper<?> mapper = model.getIntervalModel()
				.getTimelineMapper();
		final Iterator<Map<String, Object>> it = records.iterator();
		while (it.hasNext()) {
			final Map<String, Object> row = it.next();
			final Object rStart = Dates.mapToTimezone((Date) row
					.get("INTERVAL_START"), TimeZone.getDefault().getID(),
					Dates.GENERAL_TIMEZONE);
			row.put("INTERVAL_START", rStart);
			
			final Object rEnd = Dates.mapToTimezone((Date) row
					.get("INTERVAL_END"), TimeZone.getDefault().getID(),
					Dates.GENERAL_TIMEZONE);
			row.put("INTERVAL_END", rEnd);

			if (mapper.isSmallerThanStart(rEnd)
					|| mapper.isLargerThanEnd(rStart)) {
				it.remove();
				continue;
			}
		}
		this.records = records;

		// close the database
		db.shutDownDb();
	}

	/**
	 * Gets an amount of records from the database as specified by the
	 * {@code limit}.
	 * 
	 * @param limit
	 *            the amount of records to retrieve
	 * 
	 * @return the records retrieved
	 */
	public List<Map<String, Object>> getRecords(final int limit) {
		return records.subList(0, Math.min(records.size(), limit));
	}

	/**
	 * Gets all the records retrieved.
	 * 
	 * @return all the records retrieved
	 */
	public List<Map<String, Object>> getRecords() {
		return records;
	}
}
