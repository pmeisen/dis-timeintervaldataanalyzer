package net.meisen.dissertation.impl.cache;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;

public class CachingStrategy {

	/**
	 * The default {@code Clock} used if no other is defined.
	 */
	protected final static Clock defaultClock = new Clock();

	/**
	 * Weight used to identify that the entry should be removed.
	 */
	protected static int removeWeight = Integer.MIN_VALUE;

	/**
	 * The statistic defining how a particular {@code BitmapId} is used.
	 * 
	 * @author pmeisen
	 * 
	 */
	protected static class UsageStatistic {
		private final Clock clock;

		private int counter = 0;
		private long lastUsage = -1l;

		/**
		 * Default constructor initializes the {@code UsageStatistic} with 1
		 * usage and the current time as timestamp.
		 * 
		 * @param clock
		 */
		public UsageStatistic(final Clock clock) {
			this.clock = clock;

			used();
		}

		/**
		 * Increases the counter and updates the timestamp.
		 */
		public void used() {
			lastUsage = clock.getCurrentTime();

			if (counter != Integer.MAX_VALUE) {
				counter++;
			} else {
				counter = Integer.MAX_VALUE;
			}
		}

		/**
		 * The counter represents how often the {@code BitmapId} was used. The
		 * maximal value is defined by {@code Integer#MAX_VALUE}.
		 * 
		 * @return how often the {@code BitmapId} was used
		 */
		public int getCounter() {
			return counter;
		}

		/**
		 * Gets the timestamp the {@code BitmapId} was used last.
		 * 
		 * @return the timestamp the {@code BitmapId} was used last
		 */
		public long getLastUsage() {
			return lastUsage;
		}
	}

	private final Clock clock;

	private final ReentrantReadWriteLock counterLock;
	private final ReentrantReadWriteLock statLock;
	private final Map<BitmapId<?>, UsageStatistic> counter;

	private int maxCount;
	private long lastUsage;

	private int oldFactor;
	private double timeThresholdFactor;
	private double weightingTime;

	/**
	 * The constructor creates a {@code CachingStrategy} using the
	 * {@link #defaultClock} as internal {@code Clock}.
	 * 
	 * @see Clock
	 */
	public CachingStrategy() {
		this(defaultClock);
	}

	public CachingStrategy(final Clock clock) {
		this.clock = clock;

		statLock = new ReentrantReadWriteLock();
		counterLock = new ReentrantReadWriteLock();
		counter = new HashMap<BitmapId<?>, UsageStatistic>();

		this.lastUsage = clock.getCurrentTime();
		this.maxCount = 0;

		setDefaults();
	}

	public void usedBitmap(final BitmapId<?> bitmapId) {
		final boolean add;

		int count = -1;
		long usage = -1l;

		counterLock.readLock().lock();
		try {
			final UsageStatistic stat = counter.get(bitmapId);
			if (stat == null) {
				add = true;
			} else {
				add = false;

				synchronized (stat) {
					stat.used();
					count = stat.getCounter();
					usage = stat.getLastUsage();
				}
			}
		} finally {
			counterLock.readLock().unlock();
		}

		// check if we have to add the stat
		if (add) {

			// acquire write
			counterLock.writeLock().lock();
			try {

				/*
				 * check again in between some other thread could have created
				 * the stat
				 */
				UsageStatistic stat = counter.get(bitmapId);
				if (stat == null) {
					stat = new UsageStatistic(clock);

					synchronized (stat) {
						counter.put(bitmapId, stat);
						count = stat.getCounter();
						usage = stat.getLastUsage();
					}
				} else {
					synchronized (stat) {
						stat.used();
						count = stat.getCounter();
						usage = stat.getLastUsage();
					}
				}
			} finally {
				counterLock.writeLock().unlock();
			}
		}

		// update the variables
		statLock.writeLock().lock();
		try {
			if (count > maxCount) {
				maxCount = count;
			}
			if (usage > lastUsage) {
				lastUsage = usage;
			}
		} finally {
			statLock.writeLock().unlock();
		}
	}

	/**
	 * Get the maximal count measured so far.
	 * 
	 * @return the maximal count measured so far
	 */
	public int getMaxCount() {
		return maxCount;
	}

	/**
	 * Gets the last time the statistic was used.
	 * 
	 * @return the last time the statistic was used
	 */
	public long getLastUsage() {
		return lastUsage;
	}

	public List<BitmapId<?>> determineLessUsedList(final int amount,
			final boolean update) {
		final List<BitmapId<?>> lessUsed = new ArrayList<BitmapId<?>>();
		final TreeMap<Integer, Entry<BitmapId<?>, UsageStatistic>> weightedUsed = new TreeMap<Integer, Entry<BitmapId<?>, UsageStatistic>>(
				Collections.reverseOrder());

		statLock.readLock().lock();
		counterLock.readLock().lock();
		try {
			int amountOfRemoveWeight = 0;

			// iterate over the measured statistic
			for (final Entry<BitmapId<?>, UsageStatistic> e : counter
					.entrySet()) {
				final UsageStatistic stat = e.getValue();
				final int count = stat.getCounter();
				final long usage = stat.getLastUsage();
				final int weight = _weight(count, usage);

				// if the weight is a removeWeight it should be removed
				if (weight == removeWeight) {
					amountOfRemoveWeight++;
				}
				weightedUsed.put(weight, e);

				// if we found enough we can stop directly
				if (amountOfRemoveWeight == amount) {
					break;
				}
			}

			// add the needed values to the list
			int newMaxCount = 0;
			long newLastUsage = -1l;
			for (final Entry<BitmapId<?>, UsageStatistic> e : weightedUsed
					.values()) {
				if (lessUsed.size() < amount) {
					lessUsed.add(e.getKey());
				} else {
					final UsageStatistic stat = e.getValue();
					final int count = stat.getCounter();
					final long usage = stat.getLastUsage();

					// update the statistic
					if (newMaxCount < count) {
						newMaxCount = count;
					}
					if (newLastUsage < usage) {
						newLastUsage = usage;
					}

					// if we found the current values we can stop
					if (newMaxCount == maxCount && newLastUsage == lastUsage) {
						break;
					}
				}

				// set the new values
				maxCount = newMaxCount;
				lastUsage = newLastUsage < 0 ? clock.getCurrentTime()
						: newLastUsage;
			}
		} finally {
			counterLock.readLock().unlock();
			statLock.readLock().unlock();
		}

		// check if the statistic should be updated
		if (update) {
			counterLock.writeLock().lock();

			// remove all the items
			try {
				for (final BitmapId<?> id : lessUsed) {
					counter.remove(id);
				}
			} finally {
				counterLock.readLock().unlock();
			}
		}

		return lessUsed;
	}

	/**
	 * Method used to calculate the weight of a {@code BitmapId}.
	 * 
	 * @param count
	 *            the count of the {@code BitmapId} to be weighted
	 * @param lastUsage
	 *            the lastUsage of the {@code BitmapId} to be weighted
	 * 
	 * @return if the value {@link #removeWeight} is returned the entry will be
	 *         removed, otherwise a value between 0 (i.e. should be removed) and
	 *         100 (i.e. should never be removed) should be returned
	 */
	protected int _weight(final int count, final long lastUsage) {
		final long lastStatUse = getLastUsage();
		final long curTime = clock.getCurrentTime();

		/*
		 * Calculate the time elapsed since the last time the statistic was used
		 */
		final long idleStat = curTime - lastStatUse;

		/*
		 * A definition of 'Old' is needed. We assume that an element can be
		 * understood to be old if the time it stayed in the cache is
		 * oldFactor-times longer than the last refresh. We assume that a second
		 * is the shortest old we have.
		 */
		final long diffCurOld = Math.max(clock.getAmountOfASecond(), idleStat)
				* getOldFactor();

		final int deductionTime;
		if (diffCurOld > curTime) {
			deductionTime = 0;
		} else {
			final long defOfOld = curTime - diffCurOld;

			/*
			 * Check if the element is old, if so calculate a deduction which
			 * can be 100 at maximum.
			 */

			if (lastUsage <= defOfOld) {
				final long idleItem = curTime - lastUsage;
				final double deductionFactor = idleItem / diffCurOld;

				// if the data is really old, we mark it as removable
				if (deductionFactor > getTimeThresholdFactor()) {
					return removeWeight;
				} else {
					deductionTime = (int) Math.min(100, deductionFactor * 10);
				}
			} else {
				deductionTime = 0;
			}
		}

		/*
		 * Calculate the deduction of Count.
		 */
		final int deductionCount = (int) Math.max(0,
				100.0 * (1.0 - (count / getMaxCount())));

		return (int) Math.max(0, 100 - (getWeightingTime() * deductionTime)
				- ((1.0 - getWeightingTime()) * deductionCount));
	}

	/**
	 * Gets a map of each {@code BitmapId} together with the current statistic
	 * considering it's usage.
	 * 
	 * @return a map of each {@code BitmapId} together with the current
	 *         statistic considering it's usage
	 */
	public Map<BitmapId<?>, Integer> getCounts() {
		final Map<BitmapId<?>, Integer> res = new HashMap<BitmapId<?>, Integer>();

		counterLock.readLock().lock();
		try {
			for (final Entry<BitmapId<?>, UsageStatistic> e : counter
					.entrySet()) {
				res.put(e.getKey(), e.getValue().getCounter());
			}
		} finally {
			counterLock.readLock().unlock();
		}

		return res;
	}

	public int getOldFactor() {
		return oldFactor;
	}

	public void setOldFactor(final Integer oldFactor) {
		if (oldFactor == null) {
			this.oldFactor = 60;
		} else {
			this.oldFactor = oldFactor.intValue();
		}
	}

	public double getTimeThresholdFactor() {
		return timeThresholdFactor;
	}

	public void setTimeThresholdFactor(final Double timeThresholdFactor) {
		if (timeThresholdFactor == null) {
			this.timeThresholdFactor = 50.0;
		} else {
			this.timeThresholdFactor = timeThresholdFactor.doubleValue();
		}
	}

	public double getWeightingTime() {
		return weightingTime;
	}

	public void setWeightingTime(final Double weightingTime) {
		if (weightingTime == null) {
			this.weightingTime = 0.5;
		} else {
			this.weightingTime = weightingTime.doubleValue();
		}
	}

	public void setDefaults() {
		setOldFactor(null);
		setTimeThresholdFactor(null);
		setWeightingTime(null);
	}
}
