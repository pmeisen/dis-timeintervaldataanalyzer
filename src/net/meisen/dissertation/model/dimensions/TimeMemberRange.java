package net.meisen.dissertation.model.dimensions;

/**
 * A {@code TimeMemberRange} defines a range of the time-line for which is the
 * member is valid.
 * 
 * @author pmeisen
 * 
 */
public class TimeMemberRange {
	private final long start;
	private final long end;

	/**
	 * Default constructor.
	 * 
	 * @param start
	 *            the start (including)
	 * @param end
	 *            the end (including)
	 */
	public TimeMemberRange(final long start, final long end) {
		this.start = start;
		this.end = end;
	}

	/**
	 * Gets the start value.
	 * 
	 * @return the start value
	 */
	public long getStart() {
		return start;
	}

	/**
	 * Gets the end value.
	 * 
	 * @return the end value
	 */
	public long getEnd() {
		return end;
	}

	/**
	 * Gets the size of the range.
	 * 
	 * @return the size, i.e. the amount of time-points covered
	 */
	public long size() {
		return end - start + 1;
	}

	@Override
	public String toString() {
		return "[" + start + ", " + end + "]";
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == this) {
			return true;
		} else if (obj == null) {
			return false;
		} else if (obj instanceof TimeMemberRange) {
			final TimeMemberRange range = (TimeMemberRange) obj;
			return start == range.start && end == range.end;
		} else {
			return false;
		}
	}

	@Override
	public int hashCode() {
		return (int) (start ^ (start >>> 32));
	}
}
