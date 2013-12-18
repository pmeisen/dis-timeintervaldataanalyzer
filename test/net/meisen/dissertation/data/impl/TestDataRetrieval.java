package net.meisen.dissertation.data.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class TestDataRetrieval {

	private final static Db db = new Db();

	@BeforeClass
	public static void init() throws IOException {

		try {
			// add all the databases needed for the test
			db.addDb("tidaGhTasks",
					"/net/meisen/dissertation/data/impl/hsqldbs/tidaGhTasks.zip");

			// finally start it
			db.setUpDb();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Test
	public void testDb() throws SQLException {
		final List<Map<String, Object>> data = db.query("tidaGhTasks",
				"SELECT * FROM TB_DIM_LOCATION");

		assertEquals(31, data.size());
	}

	/**
	 * Make sure the database is cleanUp
	 */
	@AfterClass
	public static void cleanUp() {

		// stop the server
		db.shutDownDb();
	}
}
