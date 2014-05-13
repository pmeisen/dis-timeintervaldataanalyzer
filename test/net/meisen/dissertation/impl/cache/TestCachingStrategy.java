package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Map;

import net.meisen.dissertation.impl.cache.CachingStrategy.UsageStatistic;
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
	 * Tests the usage of the {@code CachingStrategy} in a multi-threaded
	 * environment.
	 * 
	 * @throws InterruptedException
	 *             if a thread is interrupted
	 */
	@Test
	public void testMultithreadingCount() throws InterruptedException {
		final int amountOfThreads = 1000;
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

						assertTrue(strategy.getMaxCount() <= amountOfBitmaps);
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

		assertEquals(amountOfThreads, strategy.getMaxCount());
	}

	/**
	 * Tests the maxCount value in a single threaded environment.
	 */
	@Test
	public void testMaxCount() {
		final int amountOfRuns = 10000;

		final CachingStrategy strategy = new CachingStrategy();

		for (int i = 0; i < amountOfRuns; i++) {
			strategy.usedBitmap(new BitmapId<Integer>(0, IntervalIndex.class));

			assertTrue(strategy.getMaxCount() == i + 1);
		}
	}

	/**
	 * Tests the reading of maxCount in a multi-threaded environment.
	 * 
	 * @throws InterruptedException
	 *             if a thread is interrupted
	 */
	@Test
	public void testMultithreadedMaxCount() throws InterruptedException {
		final int amountOfThreads = 1000;
		final int amountOfRuns = 10000;

		final CachingStrategy strategy = new CachingStrategy();

		// create some threads
		final Thread[] threads = new Thread[amountOfThreads];
		for (int i = 0; i < amountOfThreads; i++) {
			final int nr = i;

			threads[i] = new Thread() {

				@Override
				public void run() {
					if (nr == 0) {
						for (int i = 0; i < amountOfRuns; i++) {
							strategy.usedBitmap(new BitmapId<Integer>(0,
									IntervalIndex.class));

							assertEquals(i + 1, strategy.getMaxCount());
						}
					} else {
						strategy.usedBitmap(new BitmapId<Integer>(nr,
								IntervalIndex.class));

						while (strategy.getMaxCount() != amountOfRuns) {
							assertNotNull(strategy.getCounts());
							assertTrue(strategy.getLastUsage() > 0);

							try {
								Thread.sleep(50);
							} catch (final InterruptedException e) {
								fail(e.getMessage());
							}
						}
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
	}

	/**
	 * Helper method to create a {@code CachingStrategy}.
	 * 
	 * @param amount
	 *            the amount of entries to add to the {@code CachingStrategy}
	 * @param initValue
	 *            the timestamp to be used for initialization
	 * @param currentTime
	 *            the timestamp used for current time
	 * @param lastValueAdded
	 *            the timestamp used for the last value added
	 * 
	 * @return a {@code CachingStrategy} with the specified settings
	 */
	protected CachingStrategy createStrategy(final int amount,
			final long initValue, final long currentTime,
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

			@Override
			public long getAmountOfASecond() {
				return 1l;
			}
		});

		// add a bitmap so that we have the strategy initialized
		for (int i = 0; i < amount; i++) {
			strategy.usedBitmap(new BitmapId<Integer>(0, IntervalIndex.class));
		}

		return strategy;
	}

	/**
	 * Tests the weighting implementation of the {@code CachingStrategy}.
	 */
	@Test
	public void testWeighting() {
		CachingStrategy strategy;

		/*
		 * The strategy wasn't updated for a long time (0l), and the element was
		 * the one which updated the strategy (0l).
		 */
		strategy = createStrategy(1, 0l, 86400l, 0l);
		assertEquals(100, strategy._weight(100, 0l));

		/*
		 * The strategy was recently updated (86400l), but the element to be
		 * weighted is old (0l)
		 */
		strategy = createStrategy(1, 0l, 86400l, 86400l);
		assertEquals(CachingStrategy.removeWeight, strategy._weight(100, 0l));

		/*
		 * The strategy was shortly updated (80000l), but no element can be old
		 * based on the definition.
		 */
		strategy = createStrategy(1, 0l, 86400l, 80000l);
		assertEquals(100, strategy._weight(100, 0l));

		/*
		 * High update ratio (1l), element is therefore assumed to be really
		 * old.
		 */
		strategy = createStrategy(1, 0l, 86400l, 86399l);
		assertEquals(Integer.MIN_VALUE, strategy._weight(100, 0l));
	}
}
