package net.meisen.dissertation.impl.time.series;

import net.meisen.general.genmisc.types.Strings;

/**
 * A list of {@code TimePoint} labels, a time-point is thereby addressed by a
 * position.
 * 
 * @author pmeisen
 * 
 */
public class TimePointLabels {
	private final String[] labels;
	private final Object[] labelValues;
	private final Class<?> type;

	/**
	 * Standard constructor defining the amount of labels
	 * 
	 * @param size
	 *            the amount of labels, i.e. granules
	 * @param type
	 *            the type of the {@code labelValues}
	 */
	public TimePointLabels(final int size, final Class<?> type) {
		this.labels = new String[size];
		this.labelValues = new Object[size];
		this.type = type;
	}

	/**
	 * Gets the type of the label values.
	 * 
	 * @return the type of the label values
	 */
	public Class<?> getType() {
		return type;
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
		this.labels[pos] = label;
		this.labelValues[pos] = labelValue;
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
		return this.labelValues[pos];
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
		return this.labels[pos];
	}

	/**
	 * Gets the specified labels.
	 * 
	 * @return the specified labels
	 */
	public String[] getLabels() {
		return labels;
	}

	/**
	 * Gets the specified label-values.
	 * 
	 * @return the specified label-values
	 */
	public Object[] getLabelValues() {
		return labelValues;
	}

	@Override
	public String toString() {
		return Strings.join(",", labels);
	}

	/**
	 * Gets the amount of labels specified.
	 * 
	 * @return the amount of labels specified.
	 */
	public int size() {
		return labels.length;
	}
}
