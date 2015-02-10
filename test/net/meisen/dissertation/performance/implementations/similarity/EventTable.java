package net.meisen.dissertation.performance.implementations.similarity;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import net.meisen.general.genmisc.types.Numbers;

/**
 * An event-table of a specific size (considering the representing e-sequence
 * size). The amount of labels is dynamic, according to the requirements this is
 * necessary.
 * 
 * @author pmeisen
 * 
 */
public class EventTable {
	private final Map<List<Object>, double[]> eventTable;
	private final int size;
	private final IValueCalculator calculator;

	/**
	 * Constructor using the default count-calculator.
	 * 
	 * @param size
	 *            the size of the e-sequence to be represented
	 */
	public EventTable(final long size) {
		this(size, new IValueCalculator() {

			@Override
			public double getDefaultValue() {
				return 0.0;
			}

			@Override
			public double calcValue(final double curValue,
					final Map<String, Object> record) {
				return curValue + 1.0;
			}
		});
	}

	/**
	 * Creates a new {@code EventTable} for the specified time-points.
	 * 
	 * @param size
	 *            the size of the e-sequence to be represented
	 * @param calculator
	 *            a calculator used to determine the value
	 */
	public EventTable(final long size, final IValueCalculator calculator) {
		this.size = Numbers.castToInt(size);

		this.calculator = calculator;
		this.eventTable = new HashMap<List<Object>, double[]>();
	}

	/**
	 * Adds the event as specified by start (0-based) and end with the label,
	 * i.e. {@code [start, end]}.
	 * 
	 * @param record
	 *            the record the event is retrieved from
	 * @param start
	 *            the start
	 * @param end
	 *            the end
	 * @param values
	 *            the label
	 */
	public void addEvent(final Map<String, Object> record, final int start,
			final int end, final Object... values) {
		addEvent(record, start, end, Arrays.asList(values));
	}

	/**
	 * Adds the event as specified by start (0-based) and end with the label,
	 * i.e. {@code [start, end]}.
	 * 
	 * @param record
	 *            the record the event is retrieved from
	 * @param start
	 *            the start
	 * @param end
	 *            the end
	 * @param values
	 *            the label
	 */
	public void addEvent(final Map<String, Object> record, final int start,
			final int end, final List<Object> values) {

		// stay within the bounds
		final int s = Math.max(start, 0);
		final int e = Math.min(end, this.size - 1);

		double[] entry = this.eventTable.get(values);
		if (entry == null) {
			entry = new double[this.size];
			Arrays.fill(entry, calculator.getDefaultValue());
			this.eventTable.put(values, entry);
		}

		// set the values
		for (int i = s; i <= e; i++) {
			entry[i] = calculator.calcValue(entry[i], record);
		}
	}

	/**
	 * Sets the value for the specified label at the specified position.
	 * 
	 * @param label
	 *            the label to be set
	 * @param pos
	 *            the position to be set
	 * @param calcValue
	 *            the value to be set
	 */
	public void setValue(final List<Object> label, final int pos,
			final double calcValue) {
		double[] entry = this.eventTable.get(label);
		if (entry == null) {
			entry = new double[this.size];
			Arrays.fill(entry, calculator.getDefaultValue());
			this.eventTable.put(label, entry);
		}

		this.setValue(entry, pos, calcValue);
	}

	/**
	 * Sets the value for the specified entry at the specified position.
	 * 
	 * @param entry
	 *            the entry to set the positon at
	 * @param pos
	 *            the position to be set
	 * @param calcValue
	 *            the value to be set
	 */
	public void setValue(final double[] entry, final int pos,
			final double calcValue) {
		entry[pos] = calcValue;
	}

	/**
	 * Calculates the distance between {@code this} and the specified
	 * {@code EventTable}.
	 * 
	 * @param cmp
	 *            the table to compare with
	 * @param dt
	 *            the {@code DistanceType} to be used
	 * 
	 * @return the distance between {@code this} and the {@code cmp}
	 */
	public double distance(final EventTable cmp, final DistanceType dt) {
		if (getTimeSize() != cmp.getTimeSize()) {
			throw new IllegalStateException("Eventtable time-size is unequal ("
					+ getTimeSize() + " vs. " + cmp.getTimeSize() + ").");
		}

		final Set<List<Object>> labels = new HashSet<List<Object>>();
		labels.addAll(getLabels());
		labels.addAll(cmp.getLabels());

		return dt.apply(labels, this, cmp);
	}

	/**
	 * Gets the value (amount of active events) for the specified pos and label.
	 * 
	 * @param pos
	 *            the 0-based position
	 * @param values
	 *            the label
	 * 
	 * @return the calculated value
	 */
	public double get(final int pos, final Object... values) {
		return get(pos, Arrays.asList(values));
	}

	/**
	 * Gets the value (amount of active events) for the specified pos and label.
	 * 
	 * @param pos
	 *            the 0-based position
	 * @param values
	 *            the label
	 * 
	 * @return the calculated value
	 */
	public double get(final int pos, final List<Object> values) {
		final double[] entry = this.eventTable.get(values);
		if (entry == null) {
			return Double.NEGATIVE_INFINITY;
		} else {
			return entry[pos];
		}
	}

	/**
	 * Gets the size of the e-sequence, i.e. time.
	 * 
	 * @return the size of the e-sequence, i.e. time
	 */
	public int getTimeSize() {
		return size;
	}

	/**
	 * Gets the amounts of labels
	 * 
	 * @return the labels
	 */
	public int getLabelSize() {
		return eventTable.size();
	}

	/**
	 * Gets the labels.
	 * 
	 * @return the labels
	 */
	public Set<List<Object>> getLabels() {
		return eventTable.keySet();
	}

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		for (final Entry<List<Object>, double[]> e : eventTable.entrySet()) {
			builder.append(String.format("%1$20s: ", e.getKey()));
			for (final double val : e.getValue()) {
				builder.append(String.format("%1$4.2f ", val));
			}
		}

		return builder.toString();
	}

	/**
	 * Checks if the table is empty.
	 * 
	 * @return {@code true} if the table is empty, otherwise {@code false}
	 */
	public boolean isEmpty() {
		return eventTable.size() == 0;
	}
}
