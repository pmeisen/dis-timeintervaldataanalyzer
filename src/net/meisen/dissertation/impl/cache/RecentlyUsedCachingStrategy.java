package net.meisen.dissertation.impl.cache;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;

/**
 * Defines a {@code RecentlyUsedCachingStrategy} for the {@code FileBitmapCache}
 * . The strategy defines which elements to be cached and which once to be
 * removed if the cache is exceeded.
 * 
 * @author pmeisen
 * 
 */
public class RecentlyUsedCachingStrategy implements ICachingStrategy {

	/**
	 * The default {@code Clock} used if no other is defined.
	 */
	protected final static Clock defaultClock = new Clock();

	/**
	 * Weight used to identify that the entry should be removed.
	 */
	protected static int removeWeight = Integer.MIN_VALUE;

	private final Clock clock;

	private final ReentrantReadWriteLock counterLock;
	private final Map<BitmapId<?>, UsageStatistic> counter;
	private final LinkedList<BitmapId<?>> list;

	/**
	 * The constructor creates a {@code RecentlyUsedCachingStrategy} using the
	 * {@link #defaultClock} as internal {@code Clock}.
	 * 
	 * @see Clock
	 */
	public RecentlyUsedCachingStrategy() {
		this(defaultClock);
	}

	/**
	 * Default constructor which gets a {@code Clock} determining the time.
	 * 
	 * @param clock
	 *            the {@code Clock} used to retrieve the current time
	 */
	public RecentlyUsedCachingStrategy(final Clock clock) {
		this.clock = clock;

		counterLock = new ReentrantReadWriteLock();
		counter = new HashMap<BitmapId<?>, UsageStatistic>();
		list = new LinkedList<BitmapId<?>>();
	}

	@Override
	public void registerBitmap(final BitmapId<?> bitmapId) {
		counterLock.writeLock().lock();
		try {
			if (!counter.containsKey(bitmapId)) {
				counter.put(bitmapId, new UsageStatistic(clock));

				/*
				 * append the bitmap to the beginning of the list, because it is
				 * not really used and can be removed as soon as space is needed
				 */
				list.addFirst(bitmapId);
			}
		} finally {
			counterLock.writeLock().unlock();
		}
	}

	@Override
	public void usedBitmap(final BitmapId<?> bitmapId) {

		counterLock.writeLock().lock();
		try {

			// remove the element
			UsageStatistic stat = counter.get(bitmapId);

			// create a new one if there was none at all, otherwise use it
			if (stat == null) {
				stat = new UsageStatistic(clock);
				stat.used();

				counter.put(bitmapId, stat);
			} else {
				list.removeLastOccurrence(bitmapId);
				synchronized (stat) {
					stat.used();
				}
			}

			list.add(bitmapId);
		} finally {
			counterLock.writeLock().unlock();
		}
	}

	@Override
	public List<BitmapId<?>> determineRemovables(final Set<BitmapId<?>> set,
			final int amount, final boolean update) {
		final List<BitmapId<?>> lessUsed = new ArrayList<BitmapId<?>>();

		if (update) {
			counterLock.writeLock().lock();
			try {
				int size = Math.min(list.size(), amount);
				for (int i = 0; i < size; i++) {
					final BitmapId<?> id = list.removeFirst();
					lessUsed.add(id);
					counter.remove(id);
				}
			} finally {
				counterLock.writeLock().unlock();
			}
		} else {
			counterLock.readLock().lock();
			try {
				int size = Math.min(list.size(), amount);
				for (int i = 0; i < size; i++) {
					lessUsed.add(list.get(i));
				}
			} finally {
				counterLock.readLock().unlock();
			}
		}

		return lessUsed;
	}

	/**
	 * Gets the current statistic for the specified {@code BitmapId}.
	 * 
	 * @param id
	 *            the {@code BitmapId} to get the current statistic for
	 * 
	 * @return the current {@code UsageStatistic}
	 */
	public UsageStatistic getStatistic(final BitmapId<?> id) {
		counterLock.readLock().lock();
		try {
			final UsageStatistic stat = counter.get(id);
			final UsageStatistic clone;
			synchronized (stat) {
				clone = stat == null ? null : stat.clone();
			}
			return clone;
		} finally {
			counterLock.readLock().unlock();
		}
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

	/**
	 * Gets the amount of bitmaps statistically evaluated.
	 * 
	 * @return the amount of bitmaps statistically evaluated
	 */
	public int size() {
		counterLock.readLock().lock();
		try {
			return counter.size();
		} finally {
			counterLock.readLock().unlock();
		}
	}

	@Override
	public String toString() {
		counterLock.readLock().lock();
		try {
			return counter.toString();
		} finally {
			counterLock.readLock().unlock();
		}
	}
}
