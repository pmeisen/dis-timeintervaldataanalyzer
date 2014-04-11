package net.meisen.dissertation.impl.time.series;

import java.util.Locale;

import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.IIndexedCollection;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;

public class TimeSeriesResult {
	private final TimePointLabels labels;
	private final IIndexedCollection timeSeries;
	private final int size;

	public TimeSeriesResult(final int size, final BaseIndexFactory factory) {
		this.size = size;

		this.labels = new TimePointLabels(size);

		final IndexKeyDefinition keyDef = new IndexKeyDefinition(
				TimeSeries.class, "getId");
		this.timeSeries = factory.create(keyDef);
	}

	public void setLabel(final int pos, final String label,
			final Object labelValue) {
		labels.setLabel(pos, label, labelValue);
	}

	public void setValue(final int pos, final String timeSeriesId,
			final double value) {

		// get the indexed one
		TimeSeries series = (TimeSeries) timeSeries.getObject(timeSeriesId);

		// otherwise, let's create one
		if (series == null) {
			series = new TimeSeries(timeSeriesId, size);
			timeSeries.addObject(series);
		}

		series.setValue(pos, value);
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder();

		for (final Object o : timeSeries.getAll()) {
			final TimeSeries ts = (TimeSeries) o;

			// add each label
			for (int i = 0; i < size; i++) {
				if (i > 0) {
					sb.append("; ");
				}

				sb.append(String.format(Locale.US, "%.2f (%s)", ts.getValue(i),
						labels.getLabel(i)));
			}
			sb.append(System.getProperty("line.separator"));
		}

		return sb.toString();
	}
}
