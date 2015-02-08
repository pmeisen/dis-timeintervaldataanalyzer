package net.meisen.dissertation.performance.implementations.similarity;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
	private final Map<List<Object>, int[]> eventTablet;
	private final int size;

	/**
	 * Creates a new {@code EventTable} for the specified time-points.
	 * 
	 * @param size
	 *            the size of the e-sequence to be represented
	 */
	public EventTable(final long size) {
		this.size = Numbers.castToInt(size);

		this.eventTablet = new HashMap<List<Object>, int[]>();
	}

	/**
	 * Adds the event as specified by start (0-based) and end with the label,
	 * i.e. {@code [start, end]}.
	 * 
	 * @param start
	 *            the start
	 * @param end
	 *            the end
	 * @param values
	 *            the label
	 */
	public void addEvent(final int start, final int end, final Object... values) {
		addEvent(start, end, Arrays.asList(values));
	}

	/**
	 * Adds the event as specified by start (0-based) and end with the label,
	 * i.e. {@code [start, end]}.
	 * 
	 * @param start
	 *            the start
	 * @param end
	 *            the end
	 * @param values
	 *            the label
	 */
	public void addEvent(final int start, final int end,
			final List<Object> values) {

		// stay within the bounds
		final int s = Math.max(start, 0);
		final int e = Math.min(end, this.size - 1);

		int[] entry = this.eventTablet.get(values);
		if (entry == null) {
			entry = new int[this.size];
			this.eventTablet.put(values, entry);
		}

		// set the values
		for (int i = s; i <= e; i++) {
			entry[i]++;
		}
	}

	/**
	 * Gets the value (amount of active events) for the specified pos and label.
	 * 
	 * @param pos
	 *            the 0-based position
	 * @param values
	 *            the label
	 * 
	 * @return the amount of active events
	 */
	public int get(final int pos, final Object... values) {
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
	 * @return the amount of active events
	 */
	public int get(final int pos, final List<Object> values) {
		final int[] entry = this.eventTablet.get(values);
		if (entry == null) {
			return 0;
		} else {
			return entry[pos];
		}
	}
}
