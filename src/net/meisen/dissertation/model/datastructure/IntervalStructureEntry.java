package net.meisen.dissertation.model.datastructure;

import net.meisen.dissertation.model.datastructure.IntervalStructureEntry.IntervalEndPointTypeFactory.IntervalEndPointType;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry.IntervalTypeFactory.IntervalType;
import net.meisen.general.genmisc.types.Objects;

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
			 * Marks the entry to be the start.
			 */
			START,
			/**
			 * Marks the entry to be the end.
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

	/**
	 * Factory to create a {@code IntervalEndPointType} from a string.
	 * 
	 * @author pmeisen
	 * 
	 */
	public static class IntervalEndPointTypeFactory {

		/**
		 * The possible types for an interval-entry.
		 * 
		 * @author pmeisen
		 * 
		 */
		public static enum IntervalEndPointType {
			/**
			 * Marks the end-point to be included.
			 */
			INCLUDE,
			/**
			 * Marks the end-point to be excluded.
			 */
			EXCLUDE;
		}

		private final static IntervalEndPointType DEF_TYPE = IntervalEndPointType.INCLUDE;

		/**
		 * Determine the {@code IntervalEndPointType} for the specified string.
		 * If the string doesn't match one of the possible
		 * {@code IntervalEndPointType} enumeration the default one will be
		 * used.
		 * 
		 * @param type
		 *            the string to determine the {@code IntervalEndPointType}
		 * 
		 * @return the determined {@code IntervalEndPointType}
		 */
		public static IntervalEndPointType determine(final String type) {
			if (type == null) {
				return DEF_TYPE;
			} else if ("INCLUDE".equalsIgnoreCase(type)) {
				return IntervalEndPointType.INCLUDE;
			} else if ("EXCLUDE".equalsIgnoreCase(type)) {
				return IntervalEndPointType.EXCLUDE;
			} else {
				return DEF_TYPE;
			}
		}
	}

	private final IntervalType intervalType;
	private final IntervalEndPointType endPointType;

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
	 * A {@code IntervalStructureEntry} which is based on a {@code name} of a
	 * data element.
	 * 
	 * @param intervalType
	 *            the type of the interval entry, i.e. the start or end of the
	 *            interval
	 * @param includeEndPoint
	 *            {@code true} if the end-point is included, otherwise
	 *            {@code false}
	 * @param name
	 *            the name to be based on, should not be {@code null}
	 */
	public IntervalStructureEntry(final String intervalType,
			final boolean includeEndPoint, final String name) {
		this(IntervalTypeFactory.determine(intervalType),
				includeEndPoint ? IntervalEndPointType.INCLUDE
						: IntervalEndPointType.EXCLUDE, name, -1);
	}

	/**
	 * A {@code IntervalStructureEntry} which is based on a {@code position} of
	 * a data element.
	 * 
	 * @param intervalType
	 *            the type of the interval entry, i.e. the start or end of the
	 *            interval
	 * @param includeEndPoint
	 *            {@code true} if the end-point is included, otherwise
	 *            {@code false}
	 * @param position
	 *            the position to be based on, can a value smaller than
	 *            {@code 1} if a name is specified
	 */
	public IntervalStructureEntry(final String intervalType,
			final boolean includeEndPoint, final int position) {
		this(IntervalTypeFactory.determine(intervalType),
				includeEndPoint ? IntervalEndPointType.INCLUDE
						: IntervalEndPointType.EXCLUDE, null, position);
	}

	/**
	 * A {@code IntervalStructureEntry} which is based on a {@code position} and
	 * {@code name} of a data element. The {@code IntervalEndPointType} is
	 * determined by {@code includeEndPoint}.
	 * 
	 * @param intervalType
	 *            the type of the interval entry, i.e. the start or end of the
	 *            interval
	 * @param endPointType
	 *            the type of the end-point of the entry
	 * @param name
	 *            the name to be based on, can be {@code null} if a position
	 *            larger than {@code 0} is specified
	 * @param position
	 *            the position to be based on, can a value smaller than
	 *            {@code 1} if a name is specified
	 */
	public IntervalStructureEntry(final String intervalType,
			final String endPointType, final String name, final int position) {
		this(IntervalTypeFactory.determine(intervalType),
				IntervalEndPointTypeFactory.determine(endPointType), name,
				position);
	}

	/**
	 * A {@code IntervalStructureEntry} which is based on a {@code position} and
	 * {@code name} of a data element. The {@code IntervalEndPointType} is
	 * determined by {@code includeEndPoint}.
	 * 
	 * @param intervalType
	 *            the type of the interval entry, i.e. the start or end of the
	 *            interval
	 * @param includeEndPoint
	 *            {@code true} if the end-point is included, otherwise
	 *            {@code false}
	 * @param name
	 *            the name to be based on, can be {@code null} if a position
	 *            larger than {@code 0} is specified
	 * @param position
	 *            the position to be based on, can a value smaller than
	 *            {@code 1} if a name is specified
	 */
	public IntervalStructureEntry(final String intervalType,
			final boolean includeEndPoint, final String name, final int position) {
		this(IntervalTypeFactory.determine(intervalType),
				includeEndPoint ? IntervalEndPointType.INCLUDE
						: IntervalEndPointType.EXCLUDE, name, position);
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
	 * {@code name} of a data element. The {@code IntervalEndPointType} will be
	 * {@link IntervalEndPointType#INCLUDE}.
	 * 
	 * @param intervalType
	 *            the type of the interval entry, i.e. the start or end of the
	 *            interval
	 * @param name
	 *            the name to be based on, can be {@code null} if a position
	 *            larger than {@code 0} is specified
	 * @param position
	 *            the position to be based on, can a value smaller than
	 *            {@code 1} if a name is specified
	 */
	public IntervalStructureEntry(final IntervalType intervalType,
			final String name, final int position) {
		this(intervalType, IntervalEndPointType.INCLUDE, name, position);
	}

	/**
	 * A {@code IntervalStructureEntry} which is based on a {@code position} and
	 * {@code name} of a data element.
	 * 
	 * @param intervalType
	 *            the type of the interval entry, i.e. the start or end of the
	 *            interval
	 * @param endPointType
	 *            the type of the end-point of the entry
	 * @param name
	 *            the name to be based on, can be {@code null} if a position
	 *            larger than {@code 0} is specified
	 * @param position
	 *            the position to be based on, can a value smaller than
	 *            {@code 1} if a name is specified
	 */
	public IntervalStructureEntry(final IntervalType intervalType,
			final IntervalEndPointType endPointType, final String name,
			final int position) {
		super(name, position);

		this.endPointType = endPointType;
		this.intervalType = intervalType;
	}

	/**
	 * Gets the defined {@code IntervalType}.
	 * 
	 * @return the {@code IntervalType}
	 */
	public IntervalType getIntervalType() {
		return intervalType;
	}

	@Override
	public boolean equals(final Object o) {
		if (super.equals(o)) {
			final IntervalStructureEntry e = (IntervalStructureEntry) o;
			return Objects.equals(getIntervalType(), e.getIntervalType());
		} else {
			return false;
		}
	}

	/**
	 * Checks if {@code this} is a start-entry.
	 * 
	 * @return {@code true} if {@code this} is a start-entry, otherwise
	 *         {@code false}
	 */
	public boolean isStart() {
		return IntervalType.START.equals(intervalType);
	}

	/**
	 * Checks if {@code this} is an end-entry.
	 * 
	 * @return {@code true} if {@code this} is an end-entry, otherwise
	 *         {@code false}
	 */
	public boolean isEnd() {
		return IntervalType.END.equals(intervalType);
	}

	/**
	 * Gets the defined {@code IntervalEndPointType}.
	 * 
	 * @return the defined {@code IntervalEndPointType}
	 */
	public IntervalEndPointType getEndPointType() {
		return endPointType;
	}

	/**
	 * Checks if the end-point of the entry is defined to be inclusive (i.e.
	 * {@code true}) or exclusive (i.e. {@code false}).
	 * 
	 * @return {@code true} if the end-point is inclusive, otherwise
	 *         {@code false}
	 */
	public boolean isInclusive() {
		return endPointType.equals(IntervalEndPointType.INCLUDE);
	}
}
