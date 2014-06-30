package net.meisen.dissertation.model.indexes.datarecord;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.help.DbBasedTest;
import net.meisen.dissertation.impl.descriptors.GeneralDescriptor;
import net.meisen.dissertation.impl.persistence.FileLocation;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.descriptors.FactDescriptor;
import net.meisen.dissertation.model.descriptors.NullDescriptor;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.SliceWithDescriptors;
import net.meisen.general.genmisc.types.Numbers;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Tests the implementation of the {@code IntervalIndex}.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
@RunWith(JUnitConfigurationRunner.class)
public class TestIntervalIndex extends DbBasedTest {

	@Autowired
	private TidaModelHandler loader;

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

	private TidaModel loadModelAndData(final String dbName,
			final String dbPath, final String modelPath) throws IOException {
		final TidaModel model = loadModel(dbName, dbPath, modelPath);
		model.bulkLoadDataFromDataModel();

		return model;
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
		final TidaModel model = loadModelAndData("tidaTestDateIntervals",
				"tidaTestDateIntervals.zip", "tidaRandomDbIntervalIndex.xml");
		final IntervalIndex idx = model.getIndex().getIntervalIndex();

		// 12 Month in 10 years, random linear data
		assertEquals(120, idx.getAmountOfSlices());

		// there should be at least one value for each slice
		for (final SliceWithDescriptors<?> slice : idx.getSlices()) {
			assertTrue(slice.count() > 0);
			assertEquals(slice.numberOfModels(), 0);
		}

		// cleanUp
		model.release(true);
	}

	/**
	 * Tests the null handling of the {@code IntervalIndex}.
	 * 
	 * @throws IOException
	 *             if the db cannot be loaded
	 */
	@Test
	public void testIndexWithNullValuesBoundaries() throws IOException {
		final TidaModel model = loadModelAndData("tidaTestDateIntervals",
				"tidaTestDateIntervals.zip",
				"tidaDbWithNullIntervalIndexUsingBoundaries.xml");
		final IntervalIndex idx = model.getIndex().getIntervalIndex();

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

		// cleanUp
		model.release(true);
	}

	/**
	 * Tests the null handling of the {@code IntervalIndex}.
	 * 
	 * @throws IOException
	 *             if the db cannot be loaded
	 */
	@Test
	public void testIndexWithNullValuesOther() throws IOException {
		final TidaModel model = loadModelAndData("tidaTestDateIntervals",
				"tidaTestDateIntervals.zip",
				"tidaDbWithNullIntervalIndexUsingOther.xml");
		final IntervalIndex idx = model.getIndex().getIntervalIndex();

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
				assertEquals("at " + i, 1, slice.count());
			} else {
				assertNull("at " + i, slice);
			}
		}

		// cleanUp
		model.release(true);
	}

	/**
	 * Tests the combination and retrieval of slices.
	 * 
	 * @throws IOException
	 *             if a file cannot be read
	 */
	@Test
	public void testGetAndCombinations() throws IOException {
		final TidaModel model = loadModelAndData("tidaTestDateIntervals",
				"tidaTestDateIntervals.zip",
				"tidaDbWithNullIntervalIndexUsingOther.xml");
		final IntervalIndex idx = model.getIndex().getIntervalIndex();

		// test the type (the amount of values)
		assertEquals(Short.class, idx.getType());

		// test retrieval
		SliceWithDescriptors<?>[] slices;

		// get
		slices = idx.getSlices(60, 65);
		assertEquals(6, slices.length);
		assertNotNull(slices[0]);
		assertNull(slices[1]);
		assertNull(slices[2]);
		assertNull(slices[4]);
		assertNull(slices[5]);
		slices = idx.getSlices(100, 65);
		assertEquals(0, slices.length);

		// cleanUp
		model.release(true);
	}

	/**
	 * Tests the combination and retrieval of slices with {@code null} slices.
	 * 
	 * @throws IOException
	 *             if a file cannot be read
	 */
	@Test
	public void testGetAndCombinationsWithNulls() throws IOException {
		final TidaModel model = loadModelAndData("tidaTestDateIntervals",
				"tidaTestDateIntervals.zip",
				"tidaDbWithNullIntervalIndexUsingBoundaries.xml");
		final IntervalIndex idx = model.getIndex().getIntervalIndex();

		// test the type (the amount of values)
		assertEquals(Short.class, idx.getType());

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

		// cleanUp
		model.release(true);
	}

	/**
	 * Tests the saving and loading of a {@code IntervalIndex}.
	 * 
	 * @throws IOException
	 *             if a file cannot be read or created
	 */
	@Test
	public void testSaveAndLoad() throws IOException {
		final String dbName = "tidaTestDateIntervals";
		final String dbPath = "tidaTestDateIntervals.zip";
		final String modelPath = "tidaDbWithNullIntervalIndexUsingBoundaries.xml";

		// create an instance
		final TidaModel model = loadModel(dbName, dbPath, modelPath);
		final IntervalIndex idx = model.getIndex().getIntervalIndex();
		model.bulkLoadDataFromDataModel();
		assertEquals(676, count(idx.getSlices()));

		// create a temporary file and save it
		final String prefixFile = UUID.randomUUID().toString();
		final File tmpFile = File.createTempFile(prefixFile, ".zip");
		assertTrue(tmpFile.length() == 0);

		// create a saver instance
		loader.save(model.getId(), new FileLocation(tmpFile));
		assertTrue(tmpFile.length() > 0);

		// keep the bitmap of the loaded stuff
		final Bitmap validRecs = model.getValidRecords();
		final int lastUsedId = model.getIdentifierCache()
				.getLastUsedIdentifier();
		final int lastRecordId = model.getIndex().getLastRecordId();
		assertEquals(lastUsedId, lastRecordId);

		// remove all the models loaded so far
		loader.unloadAll();
		model.release(true);

		// load the model and check all the index specific values of the model
		final TidaModel loadedModel = loader.load(new FileLocation(tmpFile));
		assertEquals(validRecs, loadedModel.getValidRecords());
		assertEquals(lastUsedId, loadedModel.getIdentifierCache()
				.getLastUsedIdentifier());
		assertEquals(lastRecordId, loadedModel.getIndex().getLastRecordId());

		// get the index
		final IntervalIndex loadedIdx = loadedModel.getIndex()
				.getIntervalIndex();

		// make sure everything is cleaned
		assertTrue(tmpFile.delete());

		// validate the saved and loaded model
		assertEquals(676, count(loadedIdx.getSlices()));

		// test all the values
		for (final SliceWithDescriptors<?> slice : idx.getSlices()) {
			if (slice == null) {
				continue;
			}

			assertEquals(slice.getBitmap(),
					loadedIdx.getSliceById((Short) slice.getId()).getBitmap());
		}

		// cleanUp
		model.release(true);
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
		model.bulkLoadDataFromDataModel();

		final IntervalIndex idx = model.getIndex().getIntervalIndex();
		final SliceWithDescriptors<?>[] res = idx.getSlicesByTimePoints(1001,
				1050, true, true);

		assertEquals(50, res.length);
		for (int i = 0; i < 50; i++) {
			assertEquals(Numbers.castToByte(i + 1), res[i].getId());
		}

		// cleanUp
		model.release(true);
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
		model.bulkLoadDataFromDataModel();

		final IntervalIndex idx = model.getIndex().getIntervalIndex();
		final SliceWithDescriptors<?>[] res = idx.getSlicesByTimePoints(1001,
				1100, true, true);

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

		// cleanUp
		model.release(true);
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
		model.bulkLoadDataFromDataModel();

		final IntervalIndex idx = model.getIndex().getIntervalIndex();
		final SliceWithDescriptors<?>[] res = idx.getSlicesByTimePoints(1001,
				1050, false, false);

		assertEquals(48, res.length);
		for (int i = 0; i < 48; i++) {
			assertEquals(Numbers.castToByte(i + 2), res[i].getId());
		}

		// cleanUp
		model.release(true);
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
		model.bulkLoadDataFromDataModel();

		final IntervalIndex idx = model.getIndex().getIntervalIndex();
		final SliceWithDescriptors<?>[] resLeft = idx.getSlicesByTimePoints(
				1021, 1050, true, false);

		assertEquals(29, resLeft.length);
		for (int i = 0; i < 29; i++) {
			assertEquals(Numbers.castToByte(i + 21), resLeft[i].getId());
		}

		final SliceWithDescriptors<?>[] resRight = idx.getSlicesByTimePoints(
				1021, 1050, false, true);

		assertEquals(29, resRight.length);
		for (int i = 0; i < 29; i++) {
			assertEquals(Numbers.castToByte(i + 22), resRight[i].getId());
		}

		// cleanUp
		model.release(true);
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
		model.bulkLoadDataFromDataModel();

		final IntervalIndex idx = model.getIndex().getIntervalIndex();
		final SliceWithDescriptors<?>[] res = idx.getSlicesByTimePoints(1051,
				1050, true, true);

		assertEquals(0, res.length);

		// cleanUp
		model.release(true);
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
		model.bulkLoadDataFromDataModel();

		final IntervalIndex idx = model.getIndex().getIntervalIndex();
		final SliceWithDescriptors<?>[] res = idx.getSlicesByTimePoints(1050,
				1050, false, true);
		assertEquals(0, res.length);

		// cleanUp
		model.release(true);
	}

	/**
	 * Tests the association of descriptors to slices.
	 * 
	 * @throws IOException
	 *             if the file cannot be read
	 */
	@Test
	public void testDescriptorAssociation() throws IOException {
		final TidaModel model = loadModel(null, null,
				"tidaDbUsingLongIntervalsWithDescriptors.xml");
		model.bulkLoadDataFromDataModel();

		final IntervalIndex idx = model.getIndex().getIntervalIndex();
		assertIntervalsWithDescriptors(model, idx);

		// cleanUp
		model.release(true);
	}

	/**
	 * Tests the saving and loading of descriptors associated to slices.
	 * 
	 * @throws IOException
	 *             if the file cannot be read
	 */
	@Test
	public void testSaveAndLoadWithDescriptorAssociation() throws IOException {
		final TidaModel model = loadModel(null, null,
				"tidaDbUsingLongIntervalsWithDescriptors.xml");
		model.bulkLoadDataFromDataModel();

		// create a temporary file and save it
		final String prefixFile = UUID.randomUUID().toString();
		final File tmpFile = File.createTempFile(prefixFile, ".zip");
		assertTrue(tmpFile.length() == 0);

		// create a saver instance
		loader.save(model.getId(), new FileLocation(tmpFile));
		assertTrue(tmpFile.length() > 0);

		// remove all the models loaded so far
		loader.unloadAll();
		model.release(true);

		// load the model
		final TidaModel loadedModel = loader.load(new FileLocation(tmpFile));
		final IntervalIndex loadedIdx = loadedModel.getIndex()
				.getIntervalIndex();

		// make sure everything is cleaned
		assertTrue(tmpFile.delete());
		model.release(true);

		// validate the saved and loaded model
		assertIntervalsWithDescriptors(loadedModel, loadedIdx);
	}

	@SuppressWarnings("unchecked")
	private void assertIntervalsWithDescriptors(final TidaModel model,
			final IntervalIndex idx) {

		// generate equal descriptors for comparison
		final DescriptorModel<Integer> descModel = (DescriptorModel<Integer>) model
				.getMetaDataModel().getDescriptorModel("NAME");
		final NullDescriptor<Integer> nul = descModel.getNullDescriptor();
		final GeneralDescriptor<Integer> philipp = (GeneralDescriptor<Integer>) descModel
				.getDescriptorByValue("Philipp");
		final GeneralDescriptor<Integer> edison = (GeneralDescriptor<Integer>) descModel
				.getDescriptorByValue("Edison");
		final GeneralDescriptor<Integer> debbie = (GeneralDescriptor<Integer>) descModel
				.getDescriptorByValue("Debbie");

		// get all slices
		final SliceWithDescriptors<?>[] res = idx.getSlicesByTimePoints(1000,
				1059);
		assertEquals(60, res.length);

		for (int i = 0; i < res.length; i++) {
			final SliceWithDescriptors<?> slice = res[i];

			// check the model
			final List<String> models = slice.createModelList();
			assertTrue(models.contains("NAME"));
			assertEquals(1, models.size());

			final List<FactDescriptor<?>> set = slice
					.createSortedDescriptorList("NAME");
			final Set<Descriptor<?, ?, ?>> notExpected = new HashSet<Descriptor<?, ?, ?>>();
			notExpected.add(nul);
			notExpected.add(philipp);
			notExpected.add(edison);
			notExpected.add(debbie);

			// NULL is set for 1000-1015
			if (i >= 0 && i <= 15) {
				notExpected.remove(nul);
				assertTrue(nul + " contained at " + i + " (" + set + ")",
						set.contains(nul.getFactDescriptor()));
			}

			// Philipp is set for 1006-1025
			if (i >= 6 && i <= 25) {
				notExpected.remove(philipp);
				assertTrue(philipp + " contained at " + i + " (" + set + ")",
						set.contains(philipp.getFactDescriptor()));
			}

			// Edison is set for 1016-1040
			if (i >= 16 && i <= 40) {
				notExpected.remove(edison);
				assertTrue(edison + " contained at " + i + " (" + set + ")",
						set.contains(edison.getFactDescriptor()));
			}

			// Debbie is set for 1016-1040
			if (i >= 31) {
				notExpected.remove(debbie);
				assertTrue(debbie + " contained at " + i + " (" + set + ")",
						set.contains(debbie.getFactDescriptor()));
			}

			// make sure the notExpected aren't there
			for (final FactDescriptor<?> desc : set) {
				assertFalse("found " + desc + " (" + set + ")",
						notExpected.contains(desc));
			}
		}
	}

	/**
	 * Cleanup after the test
	 */
	@After
	public void unload() {
		loader.unloadAll();
	}
}
