package net.meisen.dissertation.performance.indexes;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertNotNull;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.meisen.dissertation.help.Performance;
import net.meisen.dissertation.help.Performance.ResultHolder;
import net.meisen.dissertation.model.indexes.BaseIndexedCollection;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Abstract implementation to run performance tests concerning the adding of
 * values against a {@code BaseIndexedCollection}.
 * 
 * @author pmeisen
 * 
 */
public class IndexedCollectionGetPerformance {

	@Autowired(required = false)
	private Map<String, Map<?, ?>> maps = new LinkedHashMap<String, Map<?, ?>>();

	@Autowired(required = false)
	private Map<String, BaseIndexedCollection> indexes = new LinkedHashMap<String, BaseIndexedCollection>();

	@Autowired(required = true)
	@Qualifier("dataGenerator")
	private IDataGenerator dataGenerator;

	@Autowired(required = true)
	@Qualifier("idGenerator")
	private IIdGenerator idGenerator;

	@Autowired(required = true)
	@Qualifier("amountOfData")
	private Integer amountOfData = null;

	@Autowired(required = true)
	@Qualifier("amountOfRuns")
	private Integer amountOfRuns = null;

	private ResultHolder results = new ResultHolder();

	/**
	 * Runs the defined performance tests, should be executed by the concrete
	 * test-implementation.
	 */
	public void runGetPerformanceTest() {
		for (int i = 0; i < amountOfRuns; i++) {
			runSingleGetPerformanceTest();
		}
	}

	/**
	 * Gets the results of the test.
	 * 
	 * @return the results of the test
	 */
	public ResultHolder getResults() {
		return results;
	}

	/**
	 * Prints the result of the test
	 */
	public void printResult() {
		results.print();
	}

	/**
	 * Runs all the defined tests once.
	 */
	protected void runSingleGetPerformanceTest() {

		// create a list of random values
		final Object[] ids = new Object[amountOfData];
		final Object[] values = new Object[amountOfData];
		for (int i = 0; i < amountOfData; i++) {
			values[i] = dataGenerator.generateData();
			ids[i] = idGenerator.generateId(values[i]);
		}

		// first the indexes
		for (final Entry<String, BaseIndexedCollection> entry : indexes.entrySet()) {

			if (entry.getValue().getKeyDefinition().isSingleTypedKey()) {
				runIndexGetTest(entry.getKey(), entry.getValue(), ids, values);
			}
		}

		// now the maps
		for (final Entry<String, Map<?, ?>> entry : maps.entrySet()) {
			runIndexGetTest(entry.getKey(), entry.getValue(), ids, values);
		}
	}

	/**
	 * Runs a single test, with measured times etc.
	 * 
	 * @param name
	 *            the name of the test
	 * @param idx
	 *            the index to be tested
	 * @param ids
	 *            the identifiers for the values
	 * @param values
	 *            the data to be checked for
	 */
	@SuppressWarnings("unchecked")
	protected void runIndexGetTest(final String name, final Object idx,
			final Object[] ids, final Object[] values) {

		// make sure we have something that can be tested
		final boolean isMap = idx instanceof Map;
		final boolean isIndex = idx instanceof BaseIndexedCollection;
		if (!isMap && !isIndex) {
			throw new IllegalArgumentException("Invalid index");
		}

		for (int i = 0; i < values.length; i++) {
			final Object d = values[i];

			if (isIndex) {
				assertTrue(((BaseIndexedCollection) idx).addObject(d));
			} else if (isMap) {
				final Object id = ids[i];
				assertTrue(((Map<Object, Object>) idx).put(id, d) == null);
			}
		}

		// cleanUp
		Performance.gc();

		// start the performance test
		final Performance idxPerformance = new Performance();
		idxPerformance.start();

		for (int i = 0; i < values.length; i++) {
			final Object id = ids[i];

			if (isIndex) {
				assertNotNull(((BaseIndexedCollection) idx).getObject(id));
			} else if (isMap) {
				assertNotNull(((Map<Object, Object>) idx).get(id));
			}
		}

		// finish the test
		final long[] idxResult = idxPerformance.stop();

		// add the result
		results.add(name, idxResult);

		// cleanUp
		if (isMap) {
			((Map<Integer, Integer>) idx).clear();
		} else if (isIndex) {
			((BaseIndexedCollection) idx).removeAll();
		}
	}
}
