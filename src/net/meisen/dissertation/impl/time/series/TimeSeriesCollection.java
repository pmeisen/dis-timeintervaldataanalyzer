package net.meisen.dissertation.impl.time.series;

import java.util.Collection;
import java.util.Locale;

import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.IIndexedCollection;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;

/**
 * A collection of a {@code TimeSeries} for a specified {@code interval}.
 * 
 * @author pmeisen
 * 
 */
public class TimeSeriesCollection {
	private final TimePointLabels labels;
	private final IIndexedCollection timeSeries;
	private final int size;

	/**
	 * Standard constructor, defining the size of the {@code TimeSeries} and the
	 * {@code factory} used to determine the index used for the different
	 * {@code TimeSeries} instances.
	 * 
	 * @param size
	 *            the size of the {@code TimeSeries} to be handled
	 * @param factory
	 *            the factory used to create indexes
	 */
	public TimeSeriesCollection(final int size, final BaseIndexFactory factory) {
		this.size = size;

		this.labels = new TimePointLabels(size);

		final IndexKeyDefinition keyDef = new IndexKeyDefinition(
				TimeSeries.class, "getId");
		this.timeSeries = factory.create(keyDef);
	}

	/**
	 * Sets the label for the specified {@code pos}.
	 * 
	 * @param pos
	 *            the position to set the label for
	 * @param label
	 *            the formatted label to be set
	 * @param labelValue
	 *            the original unformatted label
	 */
	public void setLabel(final int pos, final String label,
			final Object labelValue) {
		labels.setLabel(pos, label, labelValue);
	}

	/**
	 * Gets the label to be used for the specified {@code pos}. Instead of
	 * {@link #getLabel(int)} this method returns the unformatted object.
	 * 
	 * @param pos
	 *            the position to get the label for
	 * 
	 * @return the label of the specified {@code pos}
	 */
	public Object getLabelValue(final int pos) {
		return labels.getLabelValue(pos);
	}

	/**
	 * Gets the label to be used for the specified {@code pos}.
	 * 
	 * @param pos
	 *            the position to get the label for
	 * 
	 * @return the label of the specified {@code pos}
	 */
	public String getLabel(final int pos) {
		return labels.getLabel(pos);
	}

	/**
	 * Sets the {@code value} of the specified {@code timeSeriesId} at the
	 * specified {@code pos}.
	 * 
	 * @param pos
	 *            the position to set the value at
	 * @param timeSeriesId
	 *            the identifier of the {@code TimeSeries} to set the value for
	 * @param value
	 *            the value to be set
	 * 
	 * @return the old value at the specified {@code pos}
	 * 
	 * @throws IllegalArgumentException
	 *             if no {@code TimeSeries} with the specified
	 *             {@code timeSeriesId} exist
	 */
	public double setValue(final int pos, final String timeSeriesId,
			final double value) throws IllegalArgumentException {
		final TimeSeries series = getSeries(timeSeriesId);
		if (series == null) {
			throw new IllegalArgumentException(
					"A TimeSeries with the identifier '" + timeSeriesId
							+ "' does not exist.");
		}

		final double oldValue = series.getValue(pos);
		series.setValue(pos, value);

		return oldValue;
	}

	/**
	 * Gets all the {@code TimeSeries} of the collection.
	 * 
	 * @return all the {@code TimeSeries} of the collection
	 */
	@SuppressWarnings("unchecked")
	public Collection<TimeSeries> getSeries() {
		return (Collection<TimeSeries>) timeSeries.getAll();
	}

	/**
	 * Creates an empty {@code TimeSeries} for the specified
	 * {@code timeSeriesId}.
	 * 
	 * @param timeSeriesId
	 *            the identifier to be used for the created {@code TimeSeries}
	 * 
	 * @return the created {@code TimeSeries}
	 * 
	 * @throws IllegalArgumentException
	 *             if the specified {@code timeSeriesId} is used by another
	 *             {@code TimeSeries}
	 */
	public TimeSeries createSeries(final String timeSeriesId)
			throws IllegalArgumentException {

		// get the indexed one
		TimeSeries series = (TimeSeries) timeSeries.getObject(timeSeriesId);

		// otherwise, let's create one
		if (series == null) {
			series = new TimeSeries(timeSeriesId, size);
			timeSeries.addObject(series);
		} else {
			throw new IllegalArgumentException(
					"A TimeSeries with the identifier '" + timeSeriesId
							+ "' already exists.");
		}

		return series;
	}

	/**
	 * Gets the {@code TimeSeries} associated to the specified
	 * {@code timeSeriesId}. The method returns {@code null} if no
	 * {@code TimeSeries} with the specified {@code timeSeriesId} exists.
	 * 
	 * @param timeSeriesId
	 *            the identifier of the {@code TimeSeries} to be retrieved
	 * 
	 * @return the {@code TimeSeries} associated or {@code null} if no
	 *         {@code TimeSeries} exists
	 */
	public TimeSeries getSeries(final String timeSeriesId) {
		return (TimeSeries) timeSeries.getObject(timeSeriesId);
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder();

		for (final Object o : timeSeries.getAll()) {
			final TimeSeries ts = (TimeSeries) o;

			// add the id of the timeseries
			sb.append(ts.getId());
			sb.append(": ");

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

	/**
	 * Gets the amount of series handled by the result.
	 * 
	 * @return the amount of series
	 */
	public int amountOfSeries() {
		return timeSeries.size();
	}
}
