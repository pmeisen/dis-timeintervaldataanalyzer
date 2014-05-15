package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import net.meisen.dissertation.model.indexes.datarecord.IntervalIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;

import org.junit.Test;

/**
 * Tests the implementation of a {@code CachingStrategy}.
 * 
 * @author pmeisen
 * 
 */
public class TestCachingStrategy {

	/**
	 * Helper method to create a {@code CachingStrategy}.
	 * 
	 * @param amountOfBitmaps
	 *            the amount of bitmaps to be added
	 * @param amountUsed
	 *            the amount of usages of the first bitmap
	 * @param initValue
	 *            the timestamp to be used for initialization
	 * @param currentTime
	 *            the timestamp used for current time
	 * @param lastValueAdded
	 *            the timestamp used for the last value added
	 * 
	 * @return a {@code CachingStrategy} with the specified settings
	 */
	protected CachingStrategy createStrategy(final int amountOfBitmaps,
			final int amountUsed, final long initValue, final long currentTime,
			final long lastValueAdded) {
		final CachingStrategy strategy = new CachingStrategy(new Clock() {

			@Override
			public long getCurrentTime() {
				try {
					final StackTraceElement st = Thread.currentThread()
							.getStackTrace()[2];
					final Class<?> caller = Class.forName(st.getClassName());

					if (caller.equals(CachingStrategy.class)) {
						final String method = st.getMethodName();
						if (method.equals("<init>")) {
							return initValue;
						} else if (method.equals("_weight")) {
							return currentTime;
						} else {
							fail("Invalid method '" + method + "'.");
							return -1l;
						}
					} else if (caller.equals(UsageStatistic.class)) {
						// was created at the beginning of the day
						return lastValueAdded;
					} else {
						fail("Invalid caller-class '" + caller.getName() + "'.");
						return -1l;
					}
				} catch (final ClassNotFoundException e) {
					fail(e.getMessage());
					return -1l;
				}
			}
		});

		// add a bitmap so that we have the strategy initialized
		for (int id = 0; id < amountOfBitmaps; id++) {
			strategy.usedBitmap(new BitmapId<Integer>(id, IntervalIndex.class));

			if (id == 0) {
				for (int i = 0; i < amountUsed; i++) {
					strategy.usedBitmap(new BitmapId<Integer>(id,
							IntervalIndex.class));
				}
			}
		}

		return strategy;
	}

	/**
	 * Tests the usage of the {@code CachingStrategy} in a multi-threaded
	 * environment.
	 * 
	 * @throws InterruptedException
	 *             if a thread is interrupted
	 */
	@Test
	public void testMultithreadingCount() throws InterruptedException {
		final int amountOfThreads = 100;
		final int amountOfBitmaps = 10000;

		final CachingStrategy strategy = new CachingStrategy();

		// create some threads
		final Thread[] threads = new Thread[amountOfThreads];
		for (int i = 0; i < amountOfThreads; i++) {
			threads[i] = new Thread() {

				@Override
				public void run() {
					for (int i = 0; i < amountOfBitmaps; i++) {
						strategy.usedBitmap(new BitmapId<Integer>(i,
								IntervalIndex.class));
					}
				};
			};
		}

		// start all the threads
		for (int i = 0; i < amountOfThreads; i++) {
			threads[i].start();
		}

		// make sure all threads are over
		for (int i = 0; i < amountOfThreads; i++) {
			threads[i].join();
		}

		// check the result
		final Map<BitmapId<?>, Integer> counts = strategy.getCounts();
		for (final int count : counts.values()) {
			assertEquals(amountOfThreads, count);
		}
	}

	/**
	 * Determine the retrieval of the less-used elements.
	 */
	@Test
	public void testDetermineLessUsed() {
		List<Integer> expected;
		List<BitmapId<?>> l;
		CachingStrategy strategy;

		/*
		 * test an empty strategy
		 */
		strategy = new CachingStrategy();
		l = strategy.determineLessUsedList(100, true);
		assertEquals(0, l.size());
		l = strategy.determineLessUsedList(20, false);
		assertEquals(0, l.size());

		/*
		 * Test a retrieval of less elements than available without update
		 */
		strategy = createStrategy(1000, 1, 0, 0, 0);
		l = strategy.determineLessUsedList(100, false);
		assertEquals(100, l.size());
		assertEquals(1000, strategy.size());

		// create the list of expected ids
		expected = new ArrayList<Integer>();
		for (int i = 0; i < 100; i++) {
			expected.add(i);
		}

		// remove the once found
		for (final BitmapId<?> id : l) {
			assertTrue(expected.remove(id.getId()));
		}

		// everything has to be removed
		assertEquals(0, expected.size());

		/*
		 * Test a retrieval of less elements than available with update
		 */
		l = strategy.determineLessUsedList(100, true);
		assertEquals(100, l.size());
		assertEquals(900, strategy.size());

		// create the list of expected ids
		expected = new ArrayList<Integer>();
		for (int i = 0; i < 100; i++) {
			expected.add(i);
		}

		// remove the once found
		for (final BitmapId<?> id : l) {
			assertTrue("Not found: " + id.getId(), expected.remove(id.getId()));
		}

		// everything has to be removed
		assertEquals(0, expected.size());

		/*
		 * Test a retrieval of more elements than available without update
		 */
		strategy = createStrategy(5, 1, 0, 0, 0);
		l = strategy.determineLessUsedList(100, false);
		assertEquals(5, l.size());
		assertEquals(5, strategy.size());

		/*
		 * Test a retrieval of more elements than available with update
		 */
		l = strategy.determineLessUsedList(100, true);
		assertEquals(5, l.size());
		assertEquals(0, strategy.size());

		/*
		 * Test a retrieval in correct order without update
		 */
		strategy = new CachingStrategy();
		strategy.usedBitmap(new BitmapId<Integer>(0, IntervalIndex.class));
		strategy.usedBitmap(new BitmapId<Integer>(1, IntervalIndex.class));
		strategy.usedBitmap(new BitmapId<Integer>(2, IntervalIndex.class));
		strategy.usedBitmap(new BitmapId<Integer>(3, IntervalIndex.class));
		strategy.usedBitmap(new BitmapId<Integer>(4, IntervalIndex.class));
		strategy.usedBitmap(new BitmapId<Integer>(0, IntervalIndex.class));

		l = strategy.determineLessUsedList(3, false);
		assertEquals(3, l.size());
		assertEquals(1, l.get(0).getId());
		assertEquals(2, l.get(1).getId());
		assertEquals(3, l.get(2).getId());

		l = strategy.determineLessUsedList(3, true);
		assertEquals(3, l.size());
		assertEquals(2, strategy.size());
		assertEquals(1, l.get(0).getId());
		assertEquals(2, l.get(1).getId());
		assertEquals(3, l.get(2).getId());
		l = strategy.determineLessUsedList(3, true);
		assertEquals(2, l.size());
		assertEquals(0, strategy.size());
		assertEquals(4, l.get(0).getId());
		assertEquals(0, l.get(1).getId());
	}
}
