package net.meisen.dissertation.impl.time.series;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.Locale;
import java.util.Set;

import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.IIndexedCollection;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.general.genmisc.types.Numbers;

/**
 * A collection of a {@code TimeSeries} for a specified {@code interval}.
 * 
 * @author pmeisen
 * 
 */
public class TimeSeriesCollection implements Iterable<TimeSeries> {
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
	 * @param type
	 *            the type of the raw-values of the labels
	 * @param factory
	 *            the factory used to create indexes
	 */
	public TimeSeriesCollection(final int size, final Class<?> type,
			final BaseIndexFactory factory) {
		this.size = size;
		this.labels = new TimePointLabels(size, type);

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
	 * Sets the labels of the series for the specified {@code members}.
	 * 
	 * @param members
	 *            the members defining the labels
	 */
	public void setLabels(final Set<TimeLevelMember> members) {
		int i = 0;
		for (final TimeLevelMember member : members) {
			setLabel(i, member.getName(), member.getId());
			i++;
		}
	}

	/**
	 * Sets the labels for the specified {@code bounds}.
	 * 
	 * @param index
	 *            the {@code TidaIndex} to determine the values
	 * @param bounds
	 *            the start and end of the time-series
	 */
	public void setLabels(final TidaIndex index, final long[] bounds) {
		final int size = Numbers.castToInt(bounds[1] - bounds[0] + 1);

		for (int i = 0; i < size; i++) {

			// create a label for the timeSlice
			final Object timeSliceLabel = index
					.getTimePointValue(bounds[0] + i);
			final String formattedTimeSliceLabel = index
					.getTimePointLabel(timeSliceLabel);

			// set the label formatted and the real object
			setLabel(i, formattedTimeSliceLabel, timeSliceLabel);
		}
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
		final String nl = System.getProperty("line.separator");
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
			sb.append(nl);
		}

		return sb.toString().trim();
	}

	/**
	 * Gets the amount of series handled by the result.
	 * 
	 * @return the amount of series
	 */
	public int amountOfSeries() {
		return timeSeries.size();
	}

	/**
	 * Gets the labels of the collection.
	 * 
	 * @return the labels of the collection
	 */
	public String[] getLabels() {
		return labels == null ? null : labels.getLabels();
	}

	/**
	 * Gets the amount of labels specified.
	 * 
	 * @return the amount of labels specified
	 */
	public int sizeOfLabels() {
		return labels.size();
	}

	/**
	 * Gets the size of the time-series, i.e. the amount of labels, entries etc.
	 * 
	 * @return the size of the time-series, i.e. the amount of labels, entries
	 *         etc.
	 */
	public int sizeOfSeries() {
		return size;
	}

	/**
	 * Gets the amount of time-series.
	 * 
	 * @return the amount of time-series
	 */
	public int size() {
		return timeSeries.size();
	}

	@SuppressWarnings("unchecked")
	@Override
	public Iterator<TimeSeries> iterator() {
		return (Iterator<TimeSeries>) timeSeries.getAll().iterator();
	}

	/**
	 * Gets the type of the raw-values of the labels.
	 * 
	 * @return the type of the raw-values of the labels
	 */
	public Class<?> getLabelValueType() {
		return labels.getType();
	}

	@Override
	public boolean equals(final Object o) {
		if (o == this) {
			return true;
		} else if (o instanceof TimeSeriesCollection) {
			final TimeSeriesCollection tsco = (TimeSeriesCollection) o;

			// check the size
			if (size != tsco.size) {
				return false;
			}

			// check the type
			if (!getLabelValueType().equals(tsco.getLabelValueType())) {
				return false;
			}

			// check the labels
			final String[] l1 = getLabels();
			final String[] l2 = tsco.getLabels();
			if (l1 == null && l2 == null) {
				// keep going
			} else if (l1 == null || l2 == null) {
				return false;
			} else if (!Arrays.equals(tsco.getLabels(), getLabels())) {
				return false;
			}

			// check the timeseries
			if (getSeries().size() != tsco.getSeries().size()) {
				return false;
			}
			for (final TimeSeries ts : getSeries()) {
				final TimeSeries tscoTs = tsco.getSeries(ts.getId());
				if (tscoTs == null) {
					return false;
				} else if (!tscoTs.equals(ts)) {
					return false;
				}
			}

			return true;
		} else {
			return false;
		}
	}
}
