package net.meisen.dissertation.performance.iteration;

import static org.junit.Assert.assertTrue;

import java.util.Iterator;

import net.meisen.dissertation.help.Performance;
import net.meisen.dissertation.help.Performance.ResultHolder;
import net.meisen.dissertation.impl.indexes.IndexFactory;
import net.meisen.dissertation.model.indexes.IIndexedCollection;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.performance.iteration.mock.LongKey;

import org.junit.Before;
import org.junit.Test;

/**
 * Tests some iteration implementations.
 * 
 * @author pmeisen
 * 
 */
public class TestIteration {
	private final ResultHolder results = new ResultHolder();
	private final long max = 10000000;

	private IIndexedCollection idx;

	/**
	 * Creates an index to retrieve values from
	 */
	@Before
	public void createIndex() {
		final IndexFactory factory = new IndexFactory();
		final IndexKeyDefinition keyDef = new IndexKeyDefinition(LongKey.class,
				"getKey");

		idx = factory.create(keyDef);

		// add some objects
		for (long i = 0; i < max; i++) {
			idx.addObject(new LongKey(i));
		}
	}

	/**
	 * Run the different tests
	 */
	@Test
	public void runTests() {
		testForLoop();
		testIterator();

		results.print();
	}

	/**
	 * Tests the usage of a for loop
	 */
	protected void testForLoop() {

		// cleanUp
		Performance.gc();

		// start the performance test
		final Performance performance = new Performance();
		performance.start();

		for (long i = 0; i < max; i++) {
			assertTrue(idx.getObject(i) instanceof LongKey);
		}

		// finish the test
		final long[] result = performance.stop();

		// add the result
		results.add("forLoop", result);
	}

	/**
	 * Tests the usage of an {@link Iterator}.
	 */
	protected void testIterator() {
		final Iterator<Object> it = new Iterator<Object>() {

			long n = -1;

			@Override
			public boolean hasNext() {
				n++;
				return n < max;
			}

			@Override
			public Object next() {
				return idx.getObject(n);
			}

			@Override
			public void remove() {
				// not supported
			}
		};

		// cleanUp
		Performance.gc();

		// start the performance test
		final Performance performance = new Performance();
		performance.start();

		while (it.hasNext()) {
			assertTrue(it.next() instanceof LongKey);
		}

		// finish the test
		final long[] result = performance.stop();

		// add the result
		results.add("iterator", result);
	}
}
