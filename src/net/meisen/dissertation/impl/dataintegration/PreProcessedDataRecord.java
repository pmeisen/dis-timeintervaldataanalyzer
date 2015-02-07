package net.meisen.dissertation.impl.dataintegration;

import gnu.trove.map.hash.TIntObjectHashMap;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import net.meisen.dissertation.model.datasets.IDataRecord;

/**
 * A helper class to create a record which is created during the pre-processing
 * phase.
 * 
 * @author pmeisen
 * 
 */
public class PreProcessedDataRecord implements IDataRecord {

	private final IDataRecord raw;
	private final TIntObjectHashMap<String> namedPositions;
	private final Map<String, Object> namedValues;

	/**
	 * Constructor specifying the record to be pre-processed.
	 * 
	 * @param raw
	 *            the raw record
	 */
	public PreProcessedDataRecord(final IDataRecord raw) {
		this.raw = raw;
		this.namedPositions = new TIntObjectHashMap<String>();
		this.namedValues = new HashMap<String, Object>();
	}

	@Override
	public boolean hasNamedValue(final String name) {
		return namedValues.containsKey(name) || this.raw.hasNamedValue(name);
	}

	@Override
	public Object getValue(final int position) throws RuntimeException {
		final String processedName = namedPositions.get(position);

		if (processedName == null) {
			return raw.getValue(position);
		} else {
			return getValue(processedName);
		}
	}

	@Override
	public Object getValue(final String name) throws RuntimeException {
		if (namedValues.containsKey(name)) {
			return namedValues.get(name);
		} else {
			return raw.getValue(name);
		}
	}

	@Override
	public boolean isValidPosition(final int position) {
		return namedPositions.containsKey(position)
				|| raw.isValidPosition(position);
	}

	/**
	 * Sets the pre-processed value. The value is associated to a position and a
	 * name. The method checks if the provided combination is valid. If one of
	 * the position (x)or the name is not provided (i.e. {@code position < 1} or
	 * {@code name == null}), the system generates a valid value according to
	 * the provided position (x)or name.
	 * 
	 * @param position
	 *            the position the value belongs to
	 * @param name
	 *            the name the value belongs to
	 * @param value
	 *            the value to be set
	 */
	public void setValue(int position, String name, final Object value) {

		// check if the position is undefined and therefore must be determined
		if (position < 1 && name == null) {
			throw new IllegalArgumentException(
					"One of the values position or name must be specified, i.e. valid values.");
		} else if (position < 1) {
			position = getPosition(name);

			if (position < 1) {
				position = determineNewPosition();
			}
		} else if (name == null) {
			try {
				name = getName(position);
			} catch (final Exception e) {
				name = null;
			}

			if (name == null) {
				name = UUID.randomUUID().toString();
			}
		}

		// validate the setting
		final boolean oldPosValid = raw.isValidPosition(position);
		final boolean oldNameValid = raw.hasNamedValue(name);
		if (oldPosValid && oldNameValid) {
			if (!name.equals(raw.getName(position))) {
				throw new IllegalArgumentException("The specified position '"
						+ position
						+ "' does not fit the name at the position '" + name
						+ "' (found '" + raw.getName(position) + "').");
			}
		} else if (oldPosValid || oldNameValid) {
			throw new IllegalArgumentException("The position '" + position
					+ "' and name combination '" + name + "' are invalid.");
		} else if (namedPositions.containsValue(name)
				&& !name.equals(namedPositions.get(position))) {
			throw new IllegalArgumentException("The specified position '"
					+ position + "' does not fit the name at the position '"
					+ name + "'.");
		}

		final String oldName = namedPositions.put(position, name);
		if (oldName != null) {
			namedValues.remove(oldName);
		}
		namedValues.put(name, value);
	}

	/**
	 * Helper method to determine the next free-position to be used.
	 * 
	 * @return the next free position to be used
	 */
	protected int determineNewPosition() {
		int pos = 1;
		while (raw.isValidPosition(pos) || namedPositions.containsKey(pos)) {
			pos++;
		}

		return pos;
	}

	/**
	 * Sets the value for the specified position.
	 * 
	 * @param position
	 *            the position to be set
	 * @param value
	 *            the value
	 */
	public void setValue(final int position, final Object value) {
		final String name = raw.isValidPosition(position) ? raw
				.getName(position) : UUID.randomUUID().toString();
		setValue(position, name, value);
	}

	/**
	 * Sets the value for the specified name.
	 * 
	 * @param name
	 *            the name the value is associated to
	 * @param value
	 *            the value
	 */
	public void setValue(final String name, final Object value) {
		final int position = raw.getPosition(name);
		setValue(position, name, value);
	}

	@Override
	public String getName(final int position) {
		String name = namedPositions.get(position);
		if (name == null) {
			name = raw.getName(position);
		}

		return name;
	}

	@Override
	public int getPosition(final String name) {
		for (final int pos : namedPositions.keys()) {
			if (namedPositions.get(pos).equals(name)) {
				return pos;
			}
		}

		return raw.getPosition(name);
	}
}
