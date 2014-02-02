package net.meisen.dissertation.model.datastructure;

import net.meisen.dissertation.model.datastructure.IntervalStructureEntry.IntervalTypeFactory.IntervalType;

/**
 * An {@code StructureEntry} used to mark an entry to be part of the interval
 * definition within the data.
 * 
 * @author pmeisen
 * 
 */
public class IntervalStructureEntry extends StructureEntry {

	/**
	 * Factory to create a {@code IntervalType} from a string.
	 * 
	 * @author pmeisen
	 * 
	 */
	public static class IntervalTypeFactory {

		/**
		 * The possible types for an interval-entry.
		 * 
		 * @author pmeisen
		 * 
		 */
		public static enum IntervalType {
			/**
			 * Marks the entry to be the start
			 */
			START,
			/**
			 * Marks the entry to be the end
			 */
			END;
		}

		private final static IntervalType DEF_TYPE = IntervalType.START;

		/**
		 * Determine the {@code IntervalType} for the specified string. If the
		 * string doesn't match one of the possible {@code IntervalType}
		 * enumeration the default one will be used.
		 * 
		 * @param type
		 *            the string to determine the {@code IntervalType}
		 * 
		 * @return the determined {@code IntervalType}
		 */
		public static IntervalType determine(final String type) {
			if (type == null) {
				return DEF_TYPE;
			} else if ("START".equalsIgnoreCase(type)) {
				return IntervalType.START;
			} else if ("END".equalsIgnoreCase(type)) {
				return IntervalType.END;
			} else {
				return DEF_TYPE;
			}
		}
	}

	private final IntervalType type;

	/**
	 * A {@code IntervalStructureEntry} which is based on the {@code name} of a
	 * data element.
	 * 
	 * @param type
	 *            the type of the interval entry, i.e. the start or end of the
	 *            interval
	 * @param name
	 *            the name to be based on, should not be {@code null}
	 */
	public IntervalStructureEntry(final String type, final String name) {
		this(type, name, -1);
	}

	/**
	 * A {@code IntervalStructureEntry} which is based on a {@code position} of
	 * a data element.
	 * 
	 * @param type
	 *            the type of the interval entry, i.e. the start or end of the
	 *            interval
	 * @param position
	 *            the position to be based on, should be a value larger than
	 *            {@code 0}
	 */
	public IntervalStructureEntry(final String type, final int position) {
		this(type, null, position);
	}

	/**
	 * A {@code IntervalStructureEntry} which is based on a {@code position} and
	 * {@code name} of a data element.
	 * 
	 * @param type
	 *            the type of the interval entry, i.e. the start or end of the
	 *            interval
	 * @param name
	 *            the name to be based on, can be {@code null} if a position
	 *            larger than {@code 0} is specified
	 * @param position
	 *            the position to be based on, can a value smaller than
	 *            {@code 1} if a name is specified
	 */
	public IntervalStructureEntry(final String type, final String name,
			final int position) {
		this(IntervalTypeFactory.determine(type), name, position);
	}

	/**
	 * A {@code IntervalStructureEntry} which is based on the {@code name} of a
	 * data element.
	 * 
	 * @param type
	 *            the type of the interval entry, i.e. the start or end of the
	 *            interval
	 * @param name
	 *            the name to be based on, should not be {@code null}
	 */
	public IntervalStructureEntry(final IntervalType type, final String name) {
		this(type, name, -1);
	}

	/**
	 * A {@code IntervalStructureEntry} which is based on a {@code position} of
	 * a data element.
	 * 
	 * @param type
	 *            the type of the interval entry, i.e. the start or end of the
	 *            interval
	 * @param position
	 *            the position to be based on, should be a value larger than
	 *            {@code 0}
	 */
	public IntervalStructureEntry(final IntervalType type, final int position) {
		this(type, null, position);
	}

	/**
	 * A {@code IntervalStructureEntry} which is based on a {@code position} and
	 * {@code name} of a data element.
	 * 
	 * @param type
	 *            the type of the interval entry, i.e. the start or end of the
	 *            interval
	 * @param name
	 *            the name to be based on, can be {@code null} if a position
	 *            larger than {@code 0} is specified
	 * @param position
	 *            the position to be based on, can a value smaller than
	 *            {@code 1} if a name is specified
	 */
	public IntervalStructureEntry(final IntervalType type, final String name,
			final int position) {
		super(name, position);

		this.type = type;
	}

	/**
	 * Gets the defined {@code IntervalType}.
	 * 
	 * @return the {@code IntervalType}
	 */
	public IntervalType getType() {
		return type;
	}
}
