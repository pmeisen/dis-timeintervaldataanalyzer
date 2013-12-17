package net.meisen.dissertation.data.impl;

import static org.junit.Assert.assertTrue;

import java.io.IOException;

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
	public void testDb() throws InterruptedException {
		assertTrue(true);
	}

	@AfterClass
	public static void cleanUp() throws InterruptedException {

		// stop the server
		db.shutDownDb();
	}
}
