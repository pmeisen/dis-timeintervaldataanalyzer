package net.meisen.dissertation.help;

import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.lang.ref.WeakReference;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;

import net.meisen.general.genmisc.types.Dates;

/**
 * Helper class to measure the performance of an algorithm.
 * 
 * @author pmeisen
 * 
 */
public class Performance {
	private final static DecimalFormat df = new DecimalFormat(
			"###,##0.00000####");

	private final ThreadMXBean bean = ManagementFactory.getThreadMXBean();

	private long startUser = 0l;
	private long startSystem = 0l;

	/**
	 * A helper implementation to keep track of results
	 * 
	 * @author pmeisen
	 * 
	 */
	public static class ResultHolder {
		private Map<String, List<long[]>> results = new LinkedHashMap<String, List<long[]>>();

		/**
		 * Adds the result under a specified name
		 * 
		 * @param name
		 *            the name to add the result to
		 * @param result
		 *            the result to be added
		 */
		public void add(final String name, final long[] result) {
			List<long[]> list = results.get(name);
			if (list == null) {
				list = new ArrayList<long[]>();
				results.put(name, list);
			}
			list.add(result);
		}

		/**
		 * Prints the result to the console
		 */
		public void print() {

			final Map<String, Long> averages = new HashMap<String, Long>();

			for (final Entry<String, List<long[]>> r1 : results.entrySet()) {
				final String name = r1.getKey();

				System.out.println("------------- results of " + name + " ("
						+ r1.getValue().size() + " tests)" + " -------------");

				final long[] avg = Performance.calculateAverage(r1.getValue());
				final long[] min = Performance.determineMin(r1.getValue());
				final long[] max = Performance.determineMax(r1.getValue());
				System.out.println("Minimum: " + min[0]);
				System.out.println("Maximum: " + max[0]);
				System.out.println("Average: " + avg[0]);

				// add the average to the map
				averages.put(name, avg[0]);

				for (final Entry<String, List<long[]>> r2 : results.entrySet()) {
					if (r1.getKey().equals(r2.getKey())) {
						continue;
					}

					final double[] ratio = Performance.calculateRatio(
							r1.getValue(), r2.getValue());
					System.out.println(String.format(Locale.ENGLISH,
							"Ratio  : %.2f (%s)", ratio[0], r2.getKey()));
				}
			}

			// sort the runtime averages
			final List<Entry<String, Long>> sortedList = new ArrayList<Entry<String, Long>>(
					averages.entrySet());
			Collections.sort(sortedList, new Comparator<Entry<String, Long>>() {

				@Override
				public int compare(final Entry<String, Long> o1,
						final Entry<String, Long> o2) {
					return o1.getValue().compareTo(o2.getValue());
				}
			});

			// print the order list
			System.out
					.println("------------- comparison of averages -------------");
			for (int i = 0; i < sortedList.size(); i++) {
				final Entry<String, Long> e = sortedList.get(i);
				final String msg = String.format("%2d. %-40s (%.5f s)", i + 1,
						e.getKey(), (double) e.getValue() / 1000000000.0);

				System.out.println(msg);
			}
		}
	};

	/**
	 * Constructor to create the performance measuring
	 */
	public Performance() {
		reset();
	}

	/**
	 * Starts the measuring.
	 */
	public void start() {
		start(false);
	}

	/**
	 * Starts the measuring.
	 * 
	 * @param output
	 *            {@code true} if output should be written, otherwise
	 *            {@code false}
	 */
	public void start(final boolean output) {
		if (output) {
			System.out.println("Performance measuring started at: "
					+ Dates.formatDate(new Date(), "dd.MM.yyyy HH:mm:ss,SSS"));
		}

		this.startUser = getUserTime();
		this.startSystem = getCpuTime();
	}

	/**
	 * Reset the measuring
	 */
	public void reset() {
		this.startUser = 0l;
		this.startSystem = 0l;
	}

	/**
	 * Stop the measuring and get the user and system difference, i.e. runtime.
	 * 
	 * @return the runtime of user and system cpu time
	 */
	public long[] stop() {
		return stop(false);
	}

	/**
	 * Stop the measuring and get the user and system difference, i.e. runtime.
	 * 
	 * @param output
	 *            {@code true} if output should be written, otherwise
	 *            {@code false}
	 * 
	 * @return the runtime of user and system cpu time
	 */
	public long[] stop(final boolean output) {
		final long endUser = getUserTime();
		final long endSystem = getCpuTime();
		final long diffUser = endUser - this.startUser;
		final long diffSystem = endSystem - this.startSystem;

		reset();

		if (output) {
			System.out.println("Performance measuring stopped at: "
					+ Dates.formatDate(new Date(), "dd.MM.yyyy HH:mm:ss,SSS"));
		}

		return new long[] { diffUser, diffSystem };
	}

	/**
	 * Get CPU time in nanoseconds.
	 * 
	 * @return CPU time in nanoseconds
	 */
	public long getCpuTime() {
		return bean.isCurrentThreadCpuTimeSupported() ? bean
				.getCurrentThreadCpuTime() : 0L;
	}

	/**
	 * Get user time in nanoseconds.
	 * 
	 * @return user time in nanoseconds
	 */
	public long getUserTime() {
		return bean.isCurrentThreadCpuTimeSupported() ? bean
				.getCurrentThreadUserTime() : 0L;
	}

	/**
	 * Helper method to trigger the {@code GarbageCollection}. It cannot be
	 * ensured, but at least tried.
	 */
	public static void gc() {
		Object obj = new Object();

		final WeakReference<Object> ref = new WeakReference<Object>(obj);
		obj = null;
		while (ref.get() != null) {
			System.gc();
		}
	}

	/**
	 * Calculates the specified nanoseconds to seconds.
	 * 
	 * @param ns
	 *            the value to be calculated
	 * 
	 * @return the {@code ns} in {@code s}
	 */
	public double sec(final long ns) {
		return ((double) ns) / 1000000000.0;
	}

	/**
	 * Determines the minimum value out of the list of long-tuples (i.e. the
	 * tuple is the user- and system-time).
	 * 
	 * @param times
	 *            the times to get the minimum from
	 * 
	 * @return a tuple which contains the minimum user- and system-time
	 */
	public static long[] determineMin(final List<long[]> times) {

		// results of the summing
		long minUserTime = Long.MAX_VALUE;
		long minSystemTime = Long.MAX_VALUE;

		// sum it
		for (final long[] time : times) {
			if (time[0] < minUserTime) {
				minUserTime = time[0];
			}
			if (time[1] < minSystemTime) {
				minSystemTime = time[1];
			}
		}

		final long[] result = new long[2];
		result[0] = minUserTime;
		result[1] = minSystemTime;

		return result;
	}

	/**
	 * Determines the maximum value out of the list of long-tuples (i.e. the
	 * tuple is the user- and system-time).
	 * 
	 * @param times
	 *            the times to get the maximum from
	 * 
	 * @return a tuple which contains the maximum user- and system-time
	 */
	public static long[] determineMax(final List<long[]> times) {

		// results of the summing
		long maxUserTime = 0;
		long maxSystemTime = 0;

		// sum it
		for (final long[] time : times) {
			if (time[0] > maxUserTime) {
				maxUserTime = time[0];
			}
			if (time[1] > maxSystemTime) {
				maxSystemTime = time[1];
			}
		}

		final long[] result = new long[2];
		result[0] = maxUserTime;
		result[1] = maxSystemTime;

		return result;
	}

	/**
	 * Calculates the average value out of the list of long-tuples (i.e. the
	 * tuple is the user- and system-time).
	 * 
	 * @param times
	 *            the times to calculate the average from
	 * 
	 * @return a tuple which contains the average user- and system-time
	 */
	public static long[] calculateAverage(final List<long[]> times) {

		// results of the summing
		long sumUserTime = 0;
		long sumSystemTime = 0;

		// sum it
		for (final long[] time : times) {
			sumUserTime += time[0];
			sumSystemTime += time[1];
		}

		final long[] result = new long[2];
		result[0] = sumUserTime / times.size();
		result[1] = sumSystemTime / times.size();

		return result;
	}

	/**
	 * Calculates the ratio between the two long-tuples (i.e. the tuple is the
	 * user- and system-time).
	 * 
	 * @param times1
	 *            the times to calculate the ratio for
	 * @param times2
	 *            the times to calculate the ratio for
	 * 
	 * @return a tuple which contains the ratio between user- and system-time of
	 *         the two passed tuples
	 */
	public static double[] calculateRatio(final List<long[]> times1,
			final List<long[]> times2) {
		final long[] times1Avg = Performance.calculateAverage(times1);
		final long[] times2Avg = Performance.calculateAverage(times2);

		final double[] result = new double[2];
		result[0] = (double) times1Avg[0] / times2Avg[0];
		result[1] = (double) times1Avg[1] / times2Avg[1];

		return result;
	}

	/**
	 * Rounds the passed double to the specified length {@code n}
	 * 
	 * @param val
	 *            the value to be rounded
	 * @param n
	 *            the number of digest to round to
	 * 
	 * @return the rounded value
	 */
	public static double round(final double val, final int n) {
		final long zeros = (long) Math.pow(10, n);
		double roundedVal = (double) Math.round(val * zeros) / zeros;

		return roundedVal;
	}

	/**
	 * Prints the passed {@code res} as seconds.
	 * 
	 * @param res
	 *            the result to be printed
	 * 
	 * @return the printed result
	 */
	public String printSecs(final long[] res) {
		return formatSecs(sec(res[0])) + ", " + formatSecs(sec(res[1]));
	}

	/**
	 * Formats the specified seconds.
	 * 
	 * @param val
	 *            the seconds to be formatted
	 * 
	 * @return the formatted seconds
	 */
	public String formatSecs(final double val) {
		return df.format(val) + "s";
	}
}