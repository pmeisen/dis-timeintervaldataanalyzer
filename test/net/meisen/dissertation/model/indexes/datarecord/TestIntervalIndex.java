package net.meisen.dissertation.model.indexes.datarecord;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.help.ModuleAndDbBasedTest;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IClosableIterator;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.indexes.datarecord.intervalindex.ShortIntervalIndexPartition;
import net.meisen.dissertation.model.loader.TidaModelLoader;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Tests the implementation of the {@code IntervalIndex}.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TidaConfig.class)
@ContextFile("sbconfigurator-core.xml")
public class TestIntervalIndex extends ModuleAndDbBasedTest {

	@Autowired
	private TidaModelLoader loader;

	private IntervalIndex initDb(final String dbName, final String dbPath,
			final String modelPath) throws IOException {
		getDb(dbName, "/net/meisen/dissertation/impl/hsqldbs/" + dbPath);

		// load the model
		final TidaModel model = loader.load("mh_testModel",
				"/net/meisen/dissertation/model/indexes/datarecord/"
						+ modelPath);
		final IntervalIndex idx = new IntervalIndex(model);
		idx.setIntervalDataHandling(model.getIntervalDataHandling());
		final IClosableIterator<IDataRecord> it = model.getDataModel()
				.iterator();
		int i = 0;
		while (it.hasNext()) {
			final IDataRecord rec = it.next();

			// add the record
			idx.index(i, rec);
			i++;
		}
		it.close();

		return idx;
	}

	/**
	 * Tests the mapping of random database date-values.
	 * 
	 * @throws IOException
	 *             if the db cannot be loaded
	 */
	@Test
	public void testIntervalIndexFromDb() throws IOException {
		final IntervalIndex idx = initDb("tidaTestDateIntervals",
				"tidaTestDateIntervals.zip", "tidaRandomDbIntervalIndex.xml");

		// check if there is just one partition
		assertEquals(1, idx.getAmountOfPartitions());
		final IntervalIndexPartition part = idx.getPartitions().iterator()
				.next();

		// 12 Month in 10 years, random linear data
		assertEquals(120, part.getAmountOfSlices());

		// there should be at least one value for each slice
		for (final IndexDimensionSlice<?> slice : part.getSlices()) {
			assertTrue(slice.count() > 0);
		}

		loader.unloadAll();
	}

	/**
	 * Tests the null handling of the {@code IntervalIndex}.
	 * 
	 * @throws IOException
	 *             if the db cannot be loaded
	 */
	@Test
	public void testIndexWithNullValuesBoundaries() throws IOException {
		final IntervalIndex idx = initDb("tidaTestDateIntervals",
				"tidaTestDateIntervals.zip",
				"tidaDbWithNullIntervalIndexUsingBoundaries.xml");

		// check if there is just one partition
		assertEquals(1, idx.getAmountOfPartitions());
		final IntervalIndexPartition part = idx.getPartitions().iterator()
				.next();

		// test the type (the amount of values)
		assertEquals(Short.class, part.getType());
		assertTrue(part instanceof ShortIntervalIndexPartition);

		// check the results within the day
		final ShortIntervalIndexPartition shortPart = (ShortIntervalIndexPartition) part;
		assertEquals(0, shortPart.getStart());
		assertEquals(1439, shortPart.getEnd());
		for (short i = shortPart.getStart(); i < shortPart.getEnd(); i++) {
			final IndexDimensionSlice<Short> slice = shortPart.getSliceById(i);

			if (i >= 0 && i <= 60) {
				assertEquals("Bad value for " + i, 2, slice.count());
			} else if (i >= 61 && i <= 322) {
				assertEquals(1, slice.count());
			} else if (i >= 323 && i <= 1086) {
				assertNull("Found '" + i + "' with '" + slice + "'", slice);
			} else if (i >= 1087 && i <= 1439) {
				assertEquals(1, slice.count());
			} else {
				fail("Invalid value with i = " + i);
			}
		}

		loader.unloadAll();
	}

	/**
	 * Tests the null handling of the {@code IntervalIndex}.
	 * 
	 * @throws IOException
	 *             if the db cannot be loaded
	 */
	@Test
	public void testIndexWithNullValuesOther() throws IOException {
		final IntervalIndex idx = initDb("tidaTestDateIntervals",
				"tidaTestDateIntervals.zip",
				"tidaDbWithNullIntervalIndexUsingOther.xml");

		// check if there is just one partition
		assertEquals(1, idx.getAmountOfPartitions());
		final IntervalIndexPartition part = idx.getPartitions().iterator()
				.next();

		// test the type (the amount of values)
		assertEquals(Short.class, part.getType());
		assertTrue(part instanceof ShortIntervalIndexPartition);

		// check the results within the day
		final ShortIntervalIndexPartition shortPart = (ShortIntervalIndexPartition) part;
		assertEquals(0, shortPart.getStart());
		assertEquals(1439, shortPart.getEnd());
		for (short i = shortPart.getStart(); i < shortPart.getEnd(); i++) {
			final IndexDimensionSlice<Short> slice = shortPart.getSliceById(i);

			if (i >= 0 && i <= 60 || i == 300 || i == 1080) {
				assertEquals(2, slice.count());
			} else {
				assertEquals(1, slice.count());
			}
		}

		loader.unloadAll();
	}
}
