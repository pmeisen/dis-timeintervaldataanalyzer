package net.meisen.dissertation.impl.time.series;

public class TimeSeries {
	private final String id;
	private final double[] values;

	public TimeSeries(final String id, final int size) {
		this.id = id;
		this.values = new double[size];
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
}
