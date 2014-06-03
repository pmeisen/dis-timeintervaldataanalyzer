package net.meisen.dissertation.impl.datasets;

import java.util.Date;
import java.util.UUID;

/**
 * This class defines an entry of a {@code SingleStaticDataSet}. A entry is
 * thereby one value of the {@code DataSet} which is defined by it's
 * <ul>
 * <li>position</li>
 * <li>name</li>
 * <li>value</li>
 * </ul>
 * 
 * @author pmeisen
 * 
 */
public class SingleStaticDataSetEntry {
	private final int position;
	private final String name;
	private final Object value;

	/**
	 * Constructor to create an entry for a specific {@code position} with a
	 * specific {@code value}. Using this constructor will generate a random
	 * {@code name} for the entry.
	 * 
	 * @param position
	 *            the position of the entry
	 * @param value
	 *            the value of the entry
	 */
	public SingleStaticDataSetEntry(final int position, final Object value) {
		this(position, null, value);
	}

	/**
	 * This constructor defines an entry with the specified {@code name} and
	 * {@code value}. The position is undefined and has to be defined by the
	 * using-instance, i.e. by the insertion-order.
	 * 
	 * @param name
	 *            the name of the entry
	 * @param value
	 *            the value of the entry
	 */
	public SingleStaticDataSetEntry(final String name, final Object value) {
		this(-1, name, value);
	}

	/**
	 * Creates an entry with the specified {@code value}. The {@code name} will
	 * be created uniquely and randomly, the position will be defined as
	 * {@code -1}.
	 * 
	 * @param value
	 *            the value of the entry
	 */
	public SingleStaticDataSetEntry(final Object value) {
		this(-1, null, value);
	}

	/**
	 * Creates an entry with the specified {@code value}, {@code name} and
	 * {@code position}.
	 * 
	 * @param position
	 *            the position, whereby every value smaller than {@code 1} will
	 *            be mapped to {@code -1}.
	 * @param name
	 *            the name of the entry
	 * @param value
	 *            the value of the entry
	 */
	public SingleStaticDataSetEntry(final int position, final String name,
			final Object value) {
		this.position = position < 1 ? -1 : position;
		this.name = name == null ? UUID.randomUUID().toString() : name;

		if (value instanceof Date) {
			this.value = mapToTimezone((Date) value);
		} else {
			this.value = value;
		}
	}

	/**
	 * Method used to map the raw {code Date}-value to a valid {@code Date}
	 * value within the correct time-zone. The default implementation uses an
	 * identity mapping.
	 * 
	 * @param value
	 *            the value to be mapped
	 * 
	 * @return the mapped value
	 */
	protected Date mapToTimezone(final Date value) {
		return value;
	}

	/**
	 * Gets the position of the entry, which might be {@code -1}. {@code -1}
	 * defines the entry as undefined and it thereby has to be defined by the
	 * using-instance, i.e. by insertion-order.
	 * 
	 * @return the position of the entry, might be {@code -1} if the position
	 *         has to be by the using-instance
	 */
	public int getPosition() {
		return position;
	}

	/**
	 * Gets the name assigned to {@code this} entry. The name can never be
	 * {@code null}.
	 * 
	 * @return the name of the entry, which can never be {@code null}
	 */
	public String getName() {
		return name;
	}

	/**
	 * Gets the value of the entry.
	 * 
	 * @return the value of the entry
	 */
	public Object getValue() {
		return value;
	}

	@Override
	public String toString() {
		return value == null ? null : value.toString();
	}
}
