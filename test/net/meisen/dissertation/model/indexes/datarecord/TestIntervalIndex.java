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
import net.meisen.dissertation.help.DbBasedTest;
import net.meisen.dissertation.impl.persistence.FileLocation;
import net.meisen.dissertation.impl.persistence.ZipPersistor;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IClosableIterator;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.SliceWithDescriptors;
import net.meisen.dissertation.model.persistence.Group;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.genmisc.types.Numbers;
import net.meisen.general.sbconfigurator.api.IConfiguration;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the implementation of the {@code IntervalIndex}.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TidaConfig.class)
@ContextFile("sbconfigurator-core.xml")
@RunWith(JUnitConfigurationRunner.class)
public class TestIntervalIndex extends DbBasedTest {

	@Autowired
	private TidaModelHandler loader;

	@Autowired(required = true)
	@Qualifier("coreConfiguration")
	private IConfiguration configuration;

	private TidaModel loadModel(final String dbName, final String dbPath,
			final String modelPath) throws IOException {
		loader.unloadAll();

		if (dbName != null && dbPath != null) {
			getDb(dbName, "/net/meisen/dissertation/impl/hsqldbs/" + dbPath);
		}

		// load the model
		return loader
				.loadViaXslt("/net/meisen/dissertation/model/indexes/datarecord/"
						+ modelPath);
	}

	private IntervalIndex loadAndIndex(final String dbName,
			final String dbPath, final String modelPath) throws IOException {
		final TidaModel model = loadModel(dbName, dbPath, modelPath);

		// add the data to the index
		final IClosableIterator<IDataRecord> it = model.getDataModel()
				.iterator();
		while (it.hasNext()) {
			final IDataRecord rec = it.next();

			// add the record
			model.getIndex().index(rec);
		}
		it.close();

		return model.getIndex().getIntervalIndex();
	}

	private int count(final SliceWithDescriptors<?>[] slices) {
		int counter = 0;
		for (final SliceWithDescriptors<?> slice : slices) {
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
		final IntervalIndex idx = loadAndIndex("tidaTestDateIntervals",
				"tidaTestDateIntervals.zip", "tidaRandomDbIntervalIndex.xml");

		// 12 Month in 10 years, random linear data
		assertEquals(120, idx.getAmountOfSlices());

		// there should be at least one value for each slice
		for (final SliceWithDescriptors<?> slice : idx.getSlices()) {
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
		final IntervalIndex idx = loadAndIndex("tidaTestDateIntervals",
				"tidaTestDateIntervals.zip",
				"tidaDbWithNullIntervalIndexUsingBoundaries.xml");

		// test the type (the amount of values)
		assertEquals(Short.class, idx.getType());

		// check the results within the day
		assertEquals(0, idx.getNormStart());
		assertEquals(1439, idx.getNormEnd());
		for (short i = (short) idx.getNormStart(); i < idx.getNormEnd(); i++) {
			@SuppressWarnings("unchecked")
			final SliceWithDescriptors<Short> slice = (SliceWithDescriptors<Short>) idx
					.getSliceById(i);

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
		final IntervalIndex idx = loadAndIndex("tidaTestDateIntervals",
				"tidaTestDateIntervals.zip",
				"tidaDbWithNullIntervalIndexUsingOther.xml");

		// test the type (the amount of values)
		assertEquals(Short.class, idx.getType());

		// check the results within the day
		assertEquals(0, idx.getNormStart());
		assertEquals(1439, idx.getNormEnd());
		for (short i = (short) idx.getNormStart(); i < idx.getNormEnd(); i++) {
			@SuppressWarnings("unchecked")
			final SliceWithDescriptors<Short> slice = (SliceWithDescriptors<Short>) idx
					.getSliceById(i);

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
		final IntervalIndex idx = loadAndIndex("tidaTestDateIntervals",
				"tidaTestDateIntervals.zip",
				"tidaDbWithNullIntervalIndexUsingOther.xml");

		// test the type (the amount of values)
		assertEquals(Short.class, idx.getType());

		// test combination
		Bitmap bitmap;

		// or combine
		bitmap = idx.or(60, 65);
		assertEquals(2, bitmap.determineCardinality());
		bitmap = idx.or(100, 65);
		assertEquals(0, bitmap.determineCardinality());

		// and combine
		bitmap = idx.and(60, 65);
		assertEquals(1, bitmap.determineCardinality());
		bitmap = idx.and(300, 300);
		assertEquals(2, bitmap.determineCardinality());
		bitmap = idx.and(100, 65);
		assertEquals(0, bitmap.determineCardinality());

		// test retrieval
		SliceWithDescriptors<?>[] slices;

		// get
		slices = idx.getSlices(60, 65);
		assertEquals(6, slices.length);
		slices = idx.getSlices(100, 65);
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
		final IntervalIndex idx = loadAndIndex("tidaTestDateIntervals",
				"tidaTestDateIntervals.zip",
				"tidaDbWithNullIntervalIndexUsingBoundaries.xml");

		// test the type (the amount of values)
		assertEquals(Short.class, idx.getType());

		// test the logical conjunctions
		Bitmap combined;

		// or combine
		combined = idx.or(322, 340);
		assertEquals(1, combined.determineCardinality());
		combined = idx.or(323, 340);
		assertEquals(0, combined.determineCardinality());
		combined = idx.or(320, 100);
		assertEquals(0, combined.determineCardinality());

		// and combine
		combined = idx.and(320, 340);
		assertEquals(0, combined.determineCardinality());
		combined = idx.and(323, 340);
		assertEquals(0, combined.determineCardinality());
		combined = idx.and(320, 100);
		assertEquals(0, combined.determineCardinality());

		// test the slices retrieval
		SliceWithDescriptors<?>[] slices;

		// get
		slices = idx.getSlices(320, 340);
		assertEquals(21, slices.length);
		for (short i = 0; i < slices.length; i++) {
			if (i > 2) {
				assertNull(slices[i]);
			} else {
				assertTrue(slices[i].getClass().getName(),
						slices[i] instanceof SliceWithDescriptors);
				assertEquals((short) (i + 320),
						((SliceWithDescriptors<?>) slices[i]).getId());
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
		final IntervalIndex idxidx = loadAndIndex(dbName, dbPath, modelPath);
		assertEquals(676, count(idxidx.getSlices()));
		persistor.register(group, idxidx);

		// save and check
		persistor.save(new FileLocation(tmpFile));
		assertTrue(tmpFile.length() > 0);

		// unregister to make it available for a new instance
		assertEquals(idxidx, persistor.unregister(group));

		// create a loader instance
		final IntervalIndex typedIdx = loadModel(dbName, dbPath, modelPath)
				.getIndex().getIntervalIndex();
		assertEquals(0, count(typedIdx.getSlices()));
		persistor.register(group, typedIdx);

		// load the stuff
		persistor.load(new FileLocation(tmpFile));
		assertEquals(676, count(typedIdx.getSlices()));

		// test all the values
		for (final SliceWithDescriptors<?> slice : idxidx.getSlices()) {
			if (slice == null) {
				continue;
			}

			assertEquals(slice.getBitmap(),
					typedIdx.getSliceById((Short) slice.getId()).getBitmap());
		}

		assertTrue(tmpFile.delete());
	}

	/**
	 * Tests the inclusion of values using an interval {@code [a, b]}.
	 * 
	 * @throws IOException
	 *             if the file cannot be read
	 */
	@Test
	public void testInclusionAllSlicesAvailable() throws IOException {
		final TidaModel model = loadModel(null, null,
				"tidaDbUsingLongIntervals.xml");
		model.loadData();

		final IntervalIndex idx = model.getIndex().getIntervalIndex();
		final SliceWithDescriptors<?>[] res = idx.getSlices(1001, 1050, true,
				true);

		assertEquals(50, res.length);
		for (int i = 0; i < 50; i++) {
			assertEquals(Numbers.castToByte(i + 1), res[i].getId());
		}
	}

	/**
	 * Tests the inclusion of values using an interval {@code [a, b]}, whereby
	 * not all slices are available.
	 * 
	 * @throws IOException
	 *             if the file cannot be read
	 */
	@Test
	public void testInclusionSlicesPartiallyAvailable() throws IOException {
		final TidaModel model = loadModel(null, null,
				"tidaDbUsingLongIntervals.xml");
		model.loadData();

		final IntervalIndex idx = model.getIndex().getIntervalIndex();
		final SliceWithDescriptors<?>[] res = idx.getSlices(1001, 1100, true,
				true);

		/*
		 * We expect to retrieve values for the first 1001 - 1059 (1010 + 49)
		 * entries.
		 */
		assertEquals(100, res.length);
		for (int i = 0; i < 100; i++) {
			if (i < 59) {
				assertEquals(Numbers.castToByte(i + 1), res[i].getId());
			} else {
				assertNull("Not null: " + i, res[i]);
			}
		}
	}

	/**
	 * Tests the exclusion of values using an interval {@code (a, b)}.
	 * 
	 * @throws IOException
	 *             if the file cannot be read
	 */
	@Test
	public void testExclusionAllSlicesAvailable() throws IOException {
		final TidaModel model = loadModel(null, null,
				"tidaDbUsingLongIntervals.xml");
		model.loadData();

		final IntervalIndex idx = model.getIndex().getIntervalIndex();
		final SliceWithDescriptors<?>[] res = idx.getSlices(1001, 1050, false,
				false);

		assertEquals(48, res.length);
		for (int i = 0; i < 48; i++) {
			assertEquals(Numbers.castToByte(i + 2), res[i].getId());
		}
	}

	/**
	 * Tests the exclusion of values using an interval {@code (a, b]} or
	 * {@code [a, b)}.
	 * 
	 * @throws IOException
	 *             if the file cannot be read
	 */
	@Test
	public void testPartialAllSlicesAvailable() throws IOException {
		final TidaModel model = loadModel(null, null,
				"tidaDbUsingLongIntervals.xml");
		model.loadData();

		final IntervalIndex idx = model.getIndex().getIntervalIndex();
		final SliceWithDescriptors<?>[] resLeft = idx.getSlices(1021, 1050,
				true, false);

		assertEquals(29, resLeft.length);
		for (int i = 0; i < 29; i++) {
			assertEquals(Numbers.castToByte(i + 21), resLeft[i].getId());
		}

		final SliceWithDescriptors<?>[] resRight = idx.getSlices(1021, 1050,
				false, true);

		assertEquals(29, resRight.length);
		for (int i = 0; i < 29; i++) {
			assertEquals(Numbers.castToByte(i + 22), resRight[i].getId());
		}
	}

	/**
	 * Tests the data retrieval for invalid values, i.e. {@code [1000, 500]}.
	 * 
	 * @throws IOException
	 *             if the file cannot be read
	 */
	@Test
	public void testInclusionInvalidInterval() throws IOException {
		final TidaModel model = loadModel(null, null,
				"tidaDbUsingLongIntervals.xml");
		model.loadData();

		final IntervalIndex idx = model.getIndex().getIntervalIndex();
		final SliceWithDescriptors<?>[] res = idx.getSlices(1051, 1050, true,
				true);

		assertEquals(0, res.length);
	}

	/**
	 * Tests the data retrieval for invalid intervals, i.e. {@code (1050, 1050]}
	 * .
	 * 
	 * @throws IOException
	 *             if the file cannot be read
	 */
	@Test
	public void testExclusionInvalidInterval() throws IOException {
		final TidaModel model = loadModel(null, null,
				"tidaDbUsingLongIntervals.xml");
		model.loadData();

		final IntervalIndex idx = model.getIndex().getIntervalIndex();
		final SliceWithDescriptors<?>[] res = idx.getSlices(1050, 1050, false,
				true);
		assertEquals(0, res.length);
	}

	/**
	 * Cleanup after the test
	 */
	@After
	public void unload() {
		loader.unloadAll();
	}
}
