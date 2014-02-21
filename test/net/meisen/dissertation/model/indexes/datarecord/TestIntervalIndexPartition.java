package net.meisen.dissertation.model.indexes.datarecord;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.UUID;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.help.ModuleAndDbBasedTest;
import net.meisen.dissertation.impl.persistence.ZipPersistor;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IClosableIterator;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.indexes.datarecord.intervalindex.ShortIntervalIndexPartition;
import net.meisen.dissertation.model.indexes.datarecord.slices.CombinedIndexDimensionSlice;
import net.meisen.dissertation.model.indexes.datarecord.slices.IIndexDimensionSlice;
import net.meisen.dissertation.model.indexes.datarecord.slices.IndexDimensionSlice;
import net.meisen.dissertation.model.loader.TidaModelLoader;
import net.meisen.dissertation.model.persistence.Group;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Tests the implementation of the {@code IntervalIndexPartition}.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TidaConfig.class)
@ContextFile("sbconfigurator-core.xml")
public class TestIntervalIndexPartition extends ModuleAndDbBasedTest {

	@Autowired
	private TidaModelLoader loader;

	private TidaModel loadModel(final String dbName, final String dbPath,
			final String modelPath) throws IOException {
		loader.unloadAll();

		getDb(dbName, "/net/meisen/dissertation/impl/hsqldbs/" + dbPath);

		// load the model
		return loader.load(UUID.randomUUID().toString(),
				"/net/meisen/dissertation/model/indexes/datarecord/"
						+ modelPath);
	}

	private IntervalIndexPartition createPartition(final TidaModel model)
			throws IOException {
		final IntervalIndex idx = new IntervalIndex(model);
		idx.setIntervalDataHandling(model.getIntervalDataHandling());

		// check if there is just one partition
		assertEquals(1, idx.getAmountOfPartitions());
		final IntervalIndexPartition part = idx.getPartitions().iterator()
				.next();

		return part;
	}

	private IntervalIndexPartition loadAndCreate(final String dbName,
			final String dbPath, final String modelPath) throws IOException {
		final TidaModel model = loadModel(dbName, dbPath, modelPath);
		return createPartition(model);
	}

	private IntervalIndexPartition loadAndIndex(final String dbName,
			final String dbPath, final String modelPath) throws IOException {
		final TidaModel model = loadModel(dbName, dbPath, modelPath);
		final IntervalIndexPartition part = createPartition(model);

		// add the data to the partition
		final IClosableIterator<IDataRecord> it = model.getDataModel()
				.iterator();
		int i = 0;
		while (it.hasNext()) {
			final IDataRecord rec = it.next();

			// add the record
			part.index(i, rec);
			i++;
		}
		it.close();

		return part;
	}

	private int count(final IIndexDimensionSlice[] slices) {
		int counter = 0;
		for (final IIndexDimensionSlice slice : slices) {
			if (slice != null) {
				counter++;
			}
		}

		return counter;
	}

	/**
	 * Tests the mapping of random database date-values.
	 * 
	 * @throws IOException
	 *             if the db cannot be loaded
	 */
	@Test
	public void testIntervalIndexFromDb() throws IOException {
		final IntervalIndexPartition part = loadAndIndex(
				"tidaTestDateIntervals", "tidaTestDateIntervals.zip",
				"tidaRandomDbIntervalIndex.xml");

		// 12 Month in 10 years, random linear data
		assertEquals(120, part.getAmountOfSlices());

		// there should be at least one value for each slice
		for (final IndexDimensionSlice<?> slice : part.getSlices()) {
			assertTrue(slice.count() > 0);
		}
	}

	/**
	 * Tests the null handling of the {@code IntervalIndex}.
	 * 
	 * @throws IOException
	 *             if the db cannot be loaded
	 */
	@Test
	public void testIndexWithNullValuesBoundaries() throws IOException {
		final IntervalIndexPartition part = loadAndIndex(
				"tidaTestDateIntervals", "tidaTestDateIntervals.zip",
				"tidaDbWithNullIntervalIndexUsingBoundaries.xml");

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
	}

	/**
	 * Tests the null handling of the {@code IntervalIndex}.
	 * 
	 * @throws IOException
	 *             if the db cannot be loaded
	 */
	@Test
	public void testIndexWithNullValuesOther() throws IOException {
		final IntervalIndexPartition part = loadAndIndex(
				"tidaTestDateIntervals", "tidaTestDateIntervals.zip",
				"tidaDbWithNullIntervalIndexUsingOther.xml");

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
	}

	/**
	 * Tests the combination and retrieval of slices.
	 * 
	 * @throws IOException
	 *             if a file cannot be read
	 */
	@Test
	public void testGetAndCombinations() throws IOException {
		final IntervalIndexPartition part = loadAndIndex(
				"tidaTestDateIntervals", "tidaTestDateIntervals.zip",
				"tidaDbWithNullIntervalIndexUsingOther.xml");

		// test the type (the amount of values)
		assertEquals(Short.class, part.getType());
		assertTrue(part instanceof ShortIntervalIndexPartition);
		final ShortIntervalIndexPartition shortPart = (ShortIntervalIndexPartition) part;

		IIndexDimensionSlice[] slices;
		CombinedIndexDimensionSlice combined;

		// or combine
		combined = shortPart.or((short) 60, (short) 65);
		assertEquals(2, combined.count());
		combined = shortPart.or((short) 100, (short) 65);
		assertEquals(0, combined.count());

		// and combine
		combined = shortPart.and((short) 60, (short) 65);
		assertEquals(1, combined.count());
		combined = shortPart.and((short) 300, (short) 300);
		assertEquals(2, combined.count());
		combined = shortPart.and((short) 100, (short) 65);
		assertEquals(0, combined.count());

		// get
		slices = shortPart.getSlices((short) 60, (short) 65);
		assertEquals(6, slices.length);
		slices = shortPart.getSlices((short) 100, (short) 65);
		assertEquals(0, slices.length);
	}

	/**
	 * Tests the combination and retrieval of slices with {@code null} slices.
	 * 
	 * @throws IOException
	 *             if a file cannot be read
	 */
	@Test
	public void testGetAndCombinationsWithNulls() throws IOException {
		final IntervalIndexPartition part = loadAndIndex(
				"tidaTestDateIntervals", "tidaTestDateIntervals.zip",
				"tidaDbWithNullIntervalIndexUsingBoundaries.xml");

		// test the type (the amount of values)
		assertEquals(Short.class, part.getType());
		assertTrue(part instanceof ShortIntervalIndexPartition);
		final ShortIntervalIndexPartition shortPart = (ShortIntervalIndexPartition) part;

		IIndexDimensionSlice[] slices;
		CombinedIndexDimensionSlice combined;

		// or combine
		combined = shortPart.or((short) 322, (short) 340);
		assertEquals(1, combined.count());
		combined = shortPart.or((short) 323, (short) 340);
		assertEquals(0, combined.count());
		combined = shortPart.or((short) 320, (short) 100);
		assertEquals(0, combined.count());

		// and combine
		combined = shortPart.and((short) 320, (short) 340);
		assertEquals(0, combined.count());
		combined = shortPart.and((short) 323, (short) 340);
		assertEquals(0, combined.count());
		combined = shortPart.and((short) 320, (short) 100);
		assertEquals(0, combined.count());

		// get
		slices = shortPart.getSlices((short) 320, (short) 340);
		assertEquals(21, slices.length);
		for (short i = 0; i < slices.length; i++) {
			if (i > 2) {
				assertNull(slices[i]);
			} else {
				assertTrue(slices[i].getClass().getName(),
						slices[i] instanceof IndexDimensionSlice);
				assertEquals((short) (i + 320),
						((IndexDimensionSlice<?>) slices[i]).getId());
			}
		}
	}

	/**
	 * Tests the saving and loading of a {@code IntervalIndexPartition}.
	 * 
	 * @throws IOException
	 *             if a file cannot be read or created
	 */
	@Test
	public void testSaveAndLoad() throws IOException {
		final Group group = new Group("index");

		// the testing persistor we use here
		final ZipPersistor persistor = new ZipPersistor();

		// create a temporary file and save it
		final String prefixFile = UUID.randomUUID().toString();
		final File tmpFile = File.createTempFile(prefixFile, ".zip");
		assertTrue(tmpFile.length() == 0);

		// create an instance which
		final String dbName = "tidaTestDateIntervals";
		final String dbPath = "tidaTestDateIntervals.zip";
		final String modelPath = "tidaDbWithNullIntervalIndexUsingBoundaries.xml";

		// create a saver instance
		final ShortIntervalIndexPartition idxPart = (ShortIntervalIndexPartition) loadAndIndex(
				dbName, dbPath, modelPath);
		assertEquals(676, count(idxPart.getSlices()));
		persistor.register(group, idxPart);

		// save and check
		persistor.save(tmpFile.toString());
		assertTrue(tmpFile.length() > 0);

		// unregister to make it available for a new instance
		assertEquals(idxPart, persistor.unregister(group));

		// create a loader instance
		final ShortIntervalIndexPartition crtPart = (ShortIntervalIndexPartition) loadAndCreate(
				dbName, dbPath, modelPath);
		assertEquals(0, count(crtPart.getSlices()));
		persistor.register(group, crtPart);

		// load the stuff
		persistor.load(tmpFile.toString());
		assertEquals(676, count(crtPart.getSlices()));

		// test all the values
		for (final IndexDimensionSlice<?> slice : idxPart.getSlices()) {
			if (slice == null) {
				continue;
			}

			assertEquals(slice.getBitmap(),
					crtPart.getSliceById((Short) slice.getId()).getBitmap());
		}

		assertTrue(tmpFile.delete());
	}

	/**
	 * Cleanup after the test
	 */
	@After
	public void unload() {
		loader.unloadAll();
	}
}
