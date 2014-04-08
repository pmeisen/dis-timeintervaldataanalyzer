package net.meisen.dissertation.performance.indexes;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertEquals;

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
public class IndexedCollectionAddPerformance {

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
	public void runAddPerformanceTest() {
		for (int i = 0; i < amountOfRuns; i++) {
			runSingleAddPerformanceTest();
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
	protected void runSingleAddPerformanceTest() {

		// generate the values to be added
		final Object[] values = new Object[amountOfData];
		for (int i = 0; i < amountOfData; i++) {
			values[i] = dataGenerator.generateData();
		}

		// first the indexes
		for (final Entry<String, BaseIndexedCollection> entry : indexes.entrySet()) {
			runIndexAddTest(entry.getKey(), entry.getValue(), values);
		}

		// now the maps
		for (final Entry<String, Map<?, ?>> entry : maps.entrySet()) {
			runIndexAddTest(entry.getKey(), entry.getValue(), values);
		}
	}

	/**
	 * Runs a single test, with measured times etc.
	 * 
	 * @param name
	 *            the name of the test
	 * @param idx
	 *            the index to be tested
	 * @param values
	 *            the data to be added
	 */
	@SuppressWarnings("unchecked")
	protected void runIndexAddTest(final String name, final Object idx,
			final Object[] values) {

		// make sure we have something that can be tested
		final boolean isMap = idx instanceof Map;
		final boolean isIndex = idx instanceof BaseIndexedCollection;
		if (!isMap && !isIndex) {
			throw new IllegalArgumentException("Invalid index");
		}
		final int amountOfData = values.length;

		// cleanUp
		Performance.gc();

		// start the performance test
		final Performance idxPerformance = new Performance();
		idxPerformance.start();
		for (int i = 0; i < amountOfData; i++) {
			final Object d = values[i];

			if (isIndex) {
				assertTrue(((BaseIndexedCollection) idx).addObject(d));
			} else if (isMap) {
				// the idGenerator has to be as fast as the access of the
				// object's id
				final Object id = idGenerator.generateId(d);
				assertTrue(((Map<Object, Object>) idx).put(id, d) == null);
			}
		}

		// finish the test
		final long[] idxResult = idxPerformance.stop();

		// add the result
		results.add(name, idxResult);

		// cleanUp
		if (isMap) {
			assertEquals(amountOfData, ((Map<Integer, Integer>) idx).size());
			((Map<Integer, Integer>) idx).clear();
		} else if (isIndex) {
			((BaseIndexedCollection) idx).removeAll();
		}
	}
}
