package net.meisen.dissertation.performance.implementations.model;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import net.meisen.dissertation.help.Db;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.time.mapper.BaseMapper;

public class DataHolder {
	private final Db db;
	private final List<Map<String, Object>> records;

	public DataHolder(final TidaModel model) {

		// open the database
		db = new Db();
		try {
			db.addDb("tida",
					"/net/meisen/dissertation/performance/implementations/model/ghdataHsql.zip");
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
			final Object rStart = row.get("INTERVAL_START");
			final Object rEnd = row.get("INTERVAL_END");

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

	public List<Map<String, Object>> getRecords(final int limit) {
		return records.subList(0, limit);
	}

	public List<Map<String, Object>> getRecords() {
		return records;
	}
}
