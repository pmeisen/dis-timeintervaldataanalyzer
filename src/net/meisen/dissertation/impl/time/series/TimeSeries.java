package net.meisen.dissertation.impl.time.series;

import java.util.Arrays;
import java.util.Locale;

/**
 * A time-series of values retrieved from the underlying database.
 * 
 * @author pmeisen
 * 
 */
public class TimeSeries {
	private final String id;
	private final double[] values;

	/**
	 * Standard constructor defining the identifier and the size of the
	 * {@code TimeSeries}.
	 * 
	 * @param id
	 *            the identifier of the series
	 * @param size
	 *            the size, i.e. the amount of granules, of the
	 *            {@code TimeSeries}
	 */
	public TimeSeries(final String id, final int size) {
		this.id = id;
		this.values = new double[size];
		Arrays.fill(this.values, Double.NaN);
	}

	/**
	 * Gets the identifier of the {@code TimeSeries}.
	 * 
	 * @return the identifier of the {@code TimeSeries}
	 */
	public String getId() {
		return id;
	}

	/**
	 * Sets the {@code value} at the specified zero-based {@code pos}.
	 * 
	 * @param pos
	 *            the position to set the value for, zero-based
	 * @param value
	 *            the value to be set
	 */
	public void setValue(final int pos, final double value) {
		values[pos] = value;
	}

	/**
	 * Gets the value defined at the specified {@code pos}.
	 * 
	 * @param pos
	 *            the position to get the value from, zero-based
	 * 
	 * @return the value of the position
	 */
	public double getValue(final int pos) {
		return values[pos];
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder();

		// add the id of the timeseries
		sb.append(getId());
		sb.append(": ");

		// add each label
		for (int i = 0; i < values.length; i++) {
			if (i > 0) {
				sb.append("; ");
			}

			sb.append(String.format(Locale.US, "%.2f", values[i]));
		}

		return sb.toString();
	}

	/**
	 * Gets all the values of the time-series.
	 * 
	 * @return all the values of the time-series.
	 */
	public double[] getValues() {
		return values;
	}

	/**
	 * Gets the amount of granules of the series.
	 * 
	 * @return the amount of granules of the series
	 */
	public int size() {
		return values.length;
	}

	@Override
	public boolean equals(final Object o) {
		if (o == this) {
			return true;
		} else if (o instanceof TimeSeries) {
			final TimeSeries ts = (TimeSeries) o;
			return Arrays.equals(getValues(), ts.getValues());
		} else {
			return false;
		}
	}
}
