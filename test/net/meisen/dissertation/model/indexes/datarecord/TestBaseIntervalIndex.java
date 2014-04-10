package net.meisen.dissertation.model.indexes.datarecord;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.UUID;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.ModuleAndDbBasedTest;
import net.meisen.dissertation.impl.indexes.datarecord.intervalindex.ShortIntervalIndex;
import net.meisen.dissertation.impl.persistence.FileLocation;
import net.meisen.dissertation.impl.persistence.ZipPersistor;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IClosableIterator;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.IndexDimensionSlice;
import net.meisen.dissertation.model.persistence.Group;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
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
public class TestBaseIntervalIndex extends ModuleAndDbBasedTest {

	@Autowired
	private TidaModelHandler loader;

	private TidaModel loadModel(final String dbName, final String dbPath,
			final String modelPath) throws IOException {
		loader.unloadAll();

		getDb(dbName, "/net/meisen/dissertation/impl/hsqldbs/" + dbPath);

		// load the model
		return loader
				.loadViaXslt("/net/meisen/dissertation/model/indexes/datarecord/"
						+ modelPath);
	}

	private BaseIntervalIndex createIndex(final TidaModel model)
			throws IOException {
		final BaseIntervalIndex idx = model.getIntervalModel().createIndex(
				model.getDataStructure());
		idx.setIntervalDataHandling(model.getIntervalDataHandling());

		return idx;
	}

	private BaseIntervalIndex loadAndCreate(final String dbName,
			final String dbPath, final String modelPath) throws IOException {
		final TidaModel model = loadModel(dbName, dbPath, modelPath);
		return createIndex(model);
	}

	private BaseIntervalIndex loadAndIndex(final String dbName,
			final String dbPath, final String modelPath) throws IOException {
		final TidaModel model = loadModel(dbName, dbPath, modelPath);
		final BaseIntervalIndex idx = createIndex(model);

		// add the data to the index
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

	private int count(final IndexDimensionSlice<?>[] slices) {
		int counter = 0;
		for (final IndexDimensionSlice<?> slice : slices) {
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
		final BaseIntervalIndex idx = loadAndIndex("tidaTestDateIntervals",
				"tidaTestDateIntervals.zip", "tidaRandomDbIntervalIndex.xml");

		// 12 Month in 10 years, random linear data
		assertEquals(120, idx.getAmountOfSlices());

		// there should be at least one value for each slice
		for (final IndexDimensionSlice<?> slice : idx.getSlices()) {
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
		final BaseIntervalIndex idx = loadAndIndex("tidaTestDateIntervals",
				"tidaTestDateIntervals.zip",
				"tidaDbWithNullIntervalIndexUsingBoundaries.xml");

		// test the type (the amount of values)
		assertEquals(Short.class, idx.getType());
		assertTrue(idx instanceof ShortIntervalIndex);

		// check the results within the day
		final ShortIntervalIndex shortidx = (ShortIntervalIndex) idx;
		assertEquals(0, shortidx.getStart());
		assertEquals(1439, shortidx.getEnd());
		for (short i = shortidx.getStart(); i < shortidx.getEnd(); i++) {
			final IndexDimensionSlice<Short> slice = shortidx.getSliceById(i);

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
		final BaseIntervalIndex idx = loadAndIndex("tidaTestDateIntervals",
				"tidaTestDateIntervals.zip",
				"tidaDbWithNullIntervalIndexUsingOther.xml");

		// test the type (the amount of values)
		assertEquals(Short.class, idx.getType());
		assertTrue(idx instanceof ShortIntervalIndex);

		// check the results within the day
		final ShortIntervalIndex shortidx = (ShortIntervalIndex) idx;
		assertEquals(0, shortidx.getStart());
		assertEquals(1439, shortidx.getEnd());
		for (short i = shortidx.getStart(); i < shortidx.getEnd(); i++) {
			final IndexDimensionSlice<Short> slice = shortidx.getSliceById(i);

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
		final BaseIntervalIndex idx = loadAndIndex("tidaTestDateIntervals",
				"tidaTestDateIntervals.zip",
				"tidaDbWithNullIntervalIndexUsingOther.xml");

		// test the type (the amount of values)
		assertEquals(Short.class, idx.getType());
		assertTrue(idx instanceof ShortIntervalIndex);
		final ShortIntervalIndex shortidx = (ShortIntervalIndex) idx;

		// test combination
		Bitmap bitmap;

		// or combine
		bitmap = shortidx.or((short) 60, (short) 65);
		assertEquals(2, bitmap.determineCardinality());
		bitmap = shortidx.or((short) 100, (short) 65);
		assertEquals(0, bitmap.determineCardinality());

		// and combine
		bitmap = shortidx.and((short) 60, (short) 65);
		assertEquals(1, bitmap.determineCardinality());
		bitmap = shortidx.and((short) 300, (short) 300);
		assertEquals(2, bitmap.determineCardinality());
		bitmap = shortidx.and((short) 100, (short) 65);
		assertEquals(0, bitmap.determineCardinality());

		// test retrieval
		IndexDimensionSlice<?>[] slices;

		// get
		slices = shortidx.getSlices((short) 60, (short) 65);
		assertEquals(6, slices.length);
		slices = shortidx.getSlices((short) 100, (short) 65);
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
		final BaseIntervalIndex idx = loadAndIndex("tidaTestDateIntervals",
				"tidaTestDateIntervals.zip",
				"tidaDbWithNullIntervalIndexUsingBoundaries.xml");

		// test the type (the amount of values)
		assertEquals(Short.class, idx.getType());
		assertTrue(idx instanceof ShortIntervalIndex);
		final ShortIntervalIndex shortidx = (ShortIntervalIndex) idx;

		// test the logical conjunctions
		Bitmap combined;

		// or combine
		combined = shortidx.or((short) 322, (short) 340);
		assertEquals(1, combined.determineCardinality());
		combined = shortidx.or((short) 323, (short) 340);
		assertEquals(0, combined.determineCardinality());
		combined = shortidx.or((short) 320, (short) 100);
		assertEquals(0, combined.determineCardinality());

		// and combine
		combined = shortidx.and((short) 320, (short) 340);
		assertEquals(0, combined.determineCardinality());
		combined = shortidx.and((short) 323, (short) 340);
		assertEquals(0, combined.determineCardinality());
		combined = shortidx.and((short) 320, (short) 100);
		assertEquals(0, combined.determineCardinality());

		// test the slices retrieval
		IndexDimensionSlice<?>[] slices;

		// get
		slices = shortidx.getSlices((short) 320, (short) 340);
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
	 * Tests the saving and loading of a {@code IntervalIndex}.
	 * 
	 * @throws IOException
	 *             if a file cannot be read or created
	 */
	@Test
	public void testSaveAndLoad() throws IOException {
		final Group group = new Group("index");

		// the testing persistor we use here
		final ZipPersistor persistor = new ZipPersistor(
				(IExceptionRegistry) configuration
						.getModule(DefaultValues.EXCEPTIONREGISTRY_ID));

		// create a temporary file and save it
		final String prefixFile = UUID.randomUUID().toString();
		final File tmpFile = File.createTempFile(prefixFile, ".zip");
		assertTrue(tmpFile.length() == 0);

		// create an instance which
		final String dbName = "tidaTestDateIntervals";
		final String dbPath = "tidaTestDateIntervals.zip";
		final String modelPath = "tidaDbWithNullIntervalIndexUsingBoundaries.xml";

		// create a saver instance
		final ShortIntervalIndex idxidx = (ShortIntervalIndex) loadAndIndex(
				dbName, dbPath, modelPath);
		assertEquals(676, count(idxidx.getSlices()));
		persistor.register(group, idxidx);

		// save and check
		persistor.save(new FileLocation(tmpFile));
		assertTrue(tmpFile.length() > 0);

		// unregister to make it available for a new instance
		assertEquals(idxidx, persistor.unregister(group));

		// create a loader instance
		final ShortIntervalIndex typedIdx = (ShortIntervalIndex) loadAndCreate(
				dbName, dbPath, modelPath);
		assertEquals(0, count(typedIdx.getSlices()));
		persistor.register(group, typedIdx);

		// load the stuff
		persistor.load(new FileLocation(tmpFile));
		assertEquals(676, count(typedIdx.getSlices()));

		// test all the values
		for (final IndexDimensionSlice<?> slice : idxidx.getSlices()) {
			if (slice == null) {
				continue;
			}

			assertEquals(slice.getBitmap(),
					typedIdx.getSliceById((Short) slice.getId()).getBitmap());
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
