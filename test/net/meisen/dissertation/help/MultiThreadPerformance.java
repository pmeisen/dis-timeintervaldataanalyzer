package net.meisen.dissertation.help;

import java.text.DecimalFormat;
import java.util.Date;

import net.meisen.general.genmisc.types.Dates;

/**
 * Gets the time spent within all threads running.
 * 
 * @author pmeisen
 * 
 */
public class MultiThreadPerformance {
	private final static DecimalFormat df = new DecimalFormat(
			"###,##0.00000####");

	private long nanoTime = 0l;

	/**
	 * Starts the measuring
	 */
	public void start() {
		System.out.println("Performance measuring started at: "
				+ Dates.formatDate(new Date(), "dd.MM.yyyy HH:mm:ss,SSS"));
		nanoTime = System.nanoTime();
	}

	/**
	 * Stops the measuring
	 * 
	 * @return the time elapsed
	 */
	public long stop() {
		final long diff = System.nanoTime() - nanoTime;

		reset();

		System.out.println("Performance measuring stopped at: "
				+ Dates.formatDate(new Date(), "dd.MM.yyyy HH:mm:ss,SSS"));

		return diff;
	}

	/**
	 * Reset the measuring
	 */
	public void reset() {
		nanoTime = 0;
	}

	/**
	 * Prints the passed {@code res} as seconds.
	 * 
	 * @param res
	 *            the result to be printed
	 * 
	 * @return the printed result
	 */
	public String printSecs(final long res) {
		return formatSecs(sec(res));
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
