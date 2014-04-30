package net.meisen.dissertation.impl.time.series;

import java.util.Arrays;
import java.util.Locale;

public class TimeSeries {
	private final String id;
	private final double[] values;

	public TimeSeries(final String id, final int size) {
		this.id = id;
		this.values = new double[size];
		Arrays.fill(this.values, Double.NaN);
	}

	public String getId() {
		return id;
	}

	public void setValue(final int pos, final double value) {
		values[pos] = value;
	}

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

	public int size() {
		return values.length;
	}

	public void applyDefault(final double defaultValue) {
		if (Double.isNaN(defaultValue)) {
			return;
		}
		
		for (int i = 0; i < values.length; i++) {
			if (Double.isNaN(values[i])) {
				values[i] = defaultValue;
			}
		}
	}
}
