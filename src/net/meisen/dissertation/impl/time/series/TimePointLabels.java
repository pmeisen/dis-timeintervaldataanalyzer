package net.meisen.dissertation.impl.time.series;

import net.meisen.general.genmisc.types.Strings;

public class TimePointLabels {
	private final String[] labels;
	private final Object[] labelValues;

	public TimePointLabels(final int size) {
		this.labels = new String[size];
		this.labelValues = new Object[size];
	}

	public void setLabel(final int pos, final String label,
			final Object labelValue) {
		this.labels[pos] = label;
		this.labelValues[pos] = labelValue;
	}

	public Object getLabelValue(final int pos) {
		return this.labelValues[pos];
	}

	public String getLabel(final int pos) {
		return this.labels[pos];
	}

	@Override
	public String toString() {
		return Strings.join(",", labels);
	}
}
