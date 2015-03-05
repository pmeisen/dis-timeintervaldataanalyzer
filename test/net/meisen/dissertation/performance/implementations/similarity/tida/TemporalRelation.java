package net.meisen.dissertation.performance.implementations.similarity.tida;

/**
 * The different defined temporal-relations needed to determine the structure.
 * 
 * @author pmeisen
 * 
 */
public class TemporalRelation {

	/**
	 * The different start-start relations of an interval.
	 * 
	 * @author pmeisen
	 * 
	 */
	public enum StartStartRelation {
		/**
		 * Unknown indicator.
		 */
		UNKNOWN,
		/**
		 * Interval A starts with Interval B.
		 */
		STARTSWITH,
		/**
		 * Interval A starts before Interval B.
		 */
		STARTSBEFORE,
		/**
		 * Interval A starts after Interval B.
		 */
		STARTSAFTER;

		/**
		 * Swaps the direction of {@code this}.
		 * 
		 * @return the swapped direction if possible
		 */
		public StartStartRelation swap() {
			if (STARTSBEFORE.equals(this)) {
				return STARTSAFTER;
			} else if (STARTSAFTER.equals(this)) {
				return STARTSBEFORE;
			} else {
				return this;
			}
		}
	}

	/**
	 * The different end-start relations of an interval.
	 * 
	 * @author pmeisen
	 * 
	 */
	public enum EndStartRelation {
		/**
		 * Unknown indicator.
		 */
		UNKNOWN,
		/**
		 * Interval A ends with the start of B.
		 */
		ENDSWITHSTART,
		/**
		 * Interval A ends before the start of B.
		 */
		ENDSBEFORESTART,
		/**
		 * Interval A ends after the start of B.
		 */
		ENDSAFTERSTART;
	}

	/**
	 * The different end-end relations of an interval.
	 * 
	 * @author pmeisen
	 * 
	 */
	public enum EndEndRelation {
		/**
		 * Unknown indicator.
		 */
		UNKNOWN,
		/**
		 * Interval A ends with B.
		 */
		ENDSWITH,
		/**
		 * Interval A ends before B.
		 */
		ENDSBEFORE,
		/**
		 * Interval A ends after B.
		 */
		ENDSAFTER;

		/**
		 * Swaps the direction of {@code this}.
		 * 
		 * @return the swapped direction if possible
		 */
		public EndEndRelation swap() {
			if (ENDSBEFORE.equals(this)) {
				return ENDSAFTER;
			} else if (ENDSAFTER.equals(this)) {
				return ENDSBEFORE;
			} else {
				return this;
			}
		}
	}

	/**
	 * The different temporal relations needed to determine the structure.
	 * 
	 * @author pmeisen
	 * 
	 */
	public enum Relation {
		/**
		 * Interval A is before Interval B (see Allen, 1983).
		 */
		BEFORE,
		/**
		 * Interval A meets Interval B (see Allen, 1983).
		 */
		MEETS,
		/**
		 * Interval A overlaps with Interval B (see Allen, 1983).
		 */
		OVERLAPS,
		/**
		 * Interval A starts with Interval B (see Allen, 1983).
		 */
		STARTEDBY,
		/**
		 * Interval A contains Interval B (see Allen, 1983).
		 */
		CONTAINS,
		/**
		 * Interval A finishes with Interval B (see Allen, 1983).
		 */
		FINISHEDBY,
		/**
		 * Interval A is equal with Interval B (see Allen, 1983).
		 */
		COTEMPORAL,
		/**
		 * Unknown indicator.
		 */
		UNKNOWN,
		/**
		 * Invalid indicator, i.e. the value does not describe any valid
		 * temporal relation.
		 */
		INVALID;

		/**
		 * Checks if the value is calculated and unequal to unknown or invalid.
		 * 
		 * @return {@code true} if a valid value is calculated, otherwise
		 *         {@code false}
		 */
		public boolean isFinal() {
			return !INVALID.equals(this) && !UNKNOWN.equals(this);
		}
	}

	private final int startId;
	private final int endId;

	private StartStartRelation startStartRelation;
	private EndStartRelation endStartRelation;
	private EndEndRelation endEndRelation;

	private Relation relation;

	/**
	 * A class representing the {@code TemporalRelation} between two intervals A
	 * and B based on their start-start-, end-start- and end-end-relation.
	 * 
	 * @param startId
	 *            identifier of the start record
	 * @param endId
	 *            identifier of the end record
	 */
	public TemporalRelation(final int startId, final int endId) {
		this.startId = startId;
		this.endId = endId;

		startStartRelation = StartStartRelation.UNKNOWN;
		endStartRelation = EndStartRelation.UNKNOWN;
		endEndRelation = EndEndRelation.UNKNOWN;

		relation = null;
	}

	/**
	 * Gets the start-start-relation.
	 * 
	 * @return the start-start-relation
	 */
	public StartStartRelation getStartStartRelation() {
		return startStartRelation;
	}

	/**
	 * Sets the start-start-relation.
	 * 
	 * @param firstId
	 *            identifier of the start record
	 * @param secondId
	 *            identifier of the end record
	 * @param startStartRelation
	 *            the start-start-relation
	 */
	public void setStartStartRelation(final int firstId, final int secondId,
			final StartStartRelation startStartRelation) {
		relation = null;

		if (firstId == endId) {
			this.startStartRelation = startStartRelation.swap();
		} else {
			this.startStartRelation = startStartRelation;
		}
	}

	/**
	 * Sets the specified values if not {@code null}.
	 * 
	 * @param firstId
	 *            identifier of the start record
	 * @param secondId
	 *            identifier of the end record
	 * @param startStartRelation
	 *            the start-start-relation
	 * @param endStartRelation
	 *            the end-start-relation
	 * @param endEndRelation
	 *            the end-end-relation
	 * @param swappedEndStartRelation
	 *            the swapped end-start relation determines an end-start
	 *            relation if the start and end id are swapped, this does not
	 *            necessarily include a swapped logic, like with start-start-
	 *            and end-start-relations
	 */
	public void set(final int firstId, final int secondId,
			final StartStartRelation startStartRelation,
			final EndStartRelation endStartRelation,
			final EndEndRelation endEndRelation,
			final EndStartRelation swappedEndStartRelation) {

		if (startStartRelation != null) {
			setStartStartRelation(firstId, secondId, startStartRelation);
		}

		if (endStartRelation != null) {
			setEndStartRelation(firstId, secondId, endStartRelation,
					swappedEndStartRelation);
		}

		if (endEndRelation != null) {
			setEndEndRelation(firstId, secondId, endEndRelation);
		}
	}

	/**
	 * Gets the end-start-relation.
	 * 
	 * @return the start-start-relation
	 */
	public EndStartRelation getEndStartRelation() {
		return endStartRelation;
	}

	/**
	 * Sets the end-start-relation.
	 * 
	 * @param firstId
	 *            identifier of the start record
	 * @param secondId
	 *            identifier of the end record
	 * @param endStartRelation
	 *            the end-start-relation
	 */
	public void setEndStartRelation(final int firstId, final int secondId,
			final EndStartRelation endStartRelation) {
		setEndStartRelation(firstId, secondId, endStartRelation, null);
	}

	/**
	 * Sets the end-start-relation.
	 * 
	 * @param firstId
	 *            identifier of the start record
	 * @param secondId
	 *            identifier of the end record
	 * @param endStartRelation
	 *            the end-start-relation
	 * @param swappedEndStartRelation
	 *            the swapped end-start relation determines an end-start
	 *            relation if the start and end id are swapped, this does not
	 *            necessarily include a swapped logic, like with start-start-
	 *            and end-start-relations, can be {@code null} if so it's
	 *            ignored
	 */
	public void setEndStartRelation(final int firstId, final int secondId,
			final EndStartRelation endStartRelation,
			final EndStartRelation swappedEndStartRelation) {
		relation = null;

		if (firstId == endId) {
			if (swappedEndStartRelation != null) {
				this.endStartRelation = swappedEndStartRelation;
			}
		} else {
			this.endStartRelation = endStartRelation;
		}
	}

	/**
	 * Sets the end-end-relation.
	 * 
	 * @return the end-end-relation
	 */
	public EndEndRelation getEndEndRelation() {
		return endEndRelation;
	}

	/**
	 * Sets the end-end-relation.
	 * 
	 * @param firstId
	 *            identifier of the start record
	 * @param secondId
	 *            identifier of the end record
	 * @param endEndRelation
	 *            the end-end-relation
	 */
	public void setEndEndRelation(final int firstId, final int secondId,
			final EndEndRelation endEndRelation) {
		relation = null;

		if (firstId == endId) {
			this.endEndRelation = endEndRelation.swap();
		} else {
			this.endEndRelation = endEndRelation;
		}
	}

	/**
	 * Determines the temporal-relation based on the known start-start-,
	 * end-end- and end-start-relations.
	 * 
	 * @return the temporal-relation, might return {@code UNKNOWN} if one of the
	 *         values is unknown or {@code INVALID} if the combination is
	 *         invalid
	 */
	public Relation getTemporalRelation() {

		if (relation != null) {
			return relation;
		}

		relation = null;
		if (isUnknown()) {
			relation = Relation.UNKNOWN;
		} else if (StartStartRelation.STARTSWITH.equals(startStartRelation)) {
			if (EndStartRelation.ENDSAFTERSTART.equals(endStartRelation)) {
				if (EndEndRelation.ENDSWITH.equals(endEndRelation)) {
					relation = Relation.COTEMPORAL;
				} else {
					/*
					 * EndEndRelation.ENDSBEFORE.equals(endEndRelation) ||
					 * EndEndRelation.ENDSAFTER.equals(endEndRelation)
					 */
					relation = Relation.STARTEDBY;
				}
			}
		} else if (StartStartRelation.STARTSBEFORE.equals(startStartRelation)) {
			if (EndStartRelation.ENDSWITHSTART.equals(endStartRelation)) {
				if (EndEndRelation.ENDSBEFORE.equals(endEndRelation)) {
					relation = Relation.MEETS;
				}
			} else if (EndStartRelation.ENDSBEFORESTART
					.equals(endStartRelation)) {
				if (EndEndRelation.ENDSBEFORE.equals(endEndRelation)) {
					relation = Relation.BEFORE;
				}
			} else {
				/*
				 * EndStartRelation.ENDSAFTERSTART.equals(endStartRelation)
				 */
				if (EndEndRelation.ENDSBEFORE.equals(endEndRelation)) {
					relation = Relation.OVERLAPS;
				} else if (EndEndRelation.ENDSAFTER.equals(endEndRelation)) {
					relation = Relation.CONTAINS;
				} else {
					/*
					 * EndEndRelation.ENDSWITH.equals(endEndRelation)
					 */
					relation = Relation.FINISHEDBY;
				}
			}
		}

		// if we don't have one set it to invalid
		if (relation == null) {
			relation = Relation.INVALID;
		}

		return relation;
	}

	/**
	 * Checks if the result of {@code this} is still unknown, i.e. if
	 * {@link #getTemporalRelation()} will return {@link Relation#UNKNOWN}.
	 * 
	 * @return {@code true} if still unknown, otherwise {@code false}
	 */
	public boolean isUnknown() {
		return StartStartRelation.UNKNOWN.equals(startStartRelation)
				|| EndStartRelation.UNKNOWN.equals(endStartRelation)
				|| EndEndRelation.UNKNOWN.equals(endEndRelation);
	}

	@Override
	public String toString() {
		final Relation relation = getTemporalRelation();

		return startId + " " + relation + " " + endId + " ("
				+ startStartRelation + "." + endStartRelation + "."
				+ endEndRelation + ")";
	}
}
