package net.meisen.dissertation.impl.cache;

/**
 * The statistic defining how a particular {@code BitmapId} is used.
 * 
 * @author pmeisen
 * 
 */
public class UsageStatistic {
	private final Clock clock;

	private int counter = 0;
	private long lastUsage = -1l;

	/**
	 * Default constructor initializes the {@code UsageStatistic} with 0 usage
	 * and {@code -1} as timestamp.
	 * 
	 * @param clock
	 */
	public UsageStatistic(final Clock clock) {
		this.clock = clock;
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

	@Override
	public UsageStatistic clone() {
		final UsageStatistic clone = new UsageStatistic(clock);

		clone.counter = counter;
		clone.lastUsage = lastUsage;

		return clone;
	}

	@Override
	public String toString() {
		return lastUsage + " (" + counter + ")";
	}
}
