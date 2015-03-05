package net.meisen.dissertation.model.dimensions;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import net.meisen.general.genmisc.types.Objects;

/**
 * A member of a {@code TimeLevel}.
 * 
 * @author pmeisen
 * 
 * @see TimeLevel
 * 
 */
public class TimeLevelMember {

	private final String id;
	private final List<TimeMemberRange> ranges;
	private String name;

	/**
	 * Constructor to create a {@code TimeLevelMember} with the specified
	 * {@code id} and one range defined by {@code start} and {@code end}.
	 * 
	 * @param id
	 *            the identifier of the instance
	 * @param start
	 *            the start of the one range
	 * @param end
	 *            the end of the one range
	 */
	public TimeLevelMember(final String id, final long start, final long end) {
		this(id, new TimeMemberRange(start, end));
	}

	/**
	 * Constructor to create a {@code TimeLevelMember} with the specified
	 * {@code id} and {@code ranges}.
	 * 
	 * @param id
	 *            the identifier of the instance
	 * @param ranges
	 *            the ranges defined for {@code this}
	 */
	public TimeLevelMember(final String id, final TimeMemberRange... ranges) {
		this(id, ranges == null ? null : Arrays.asList(ranges));
	}

	/**
	 * Constructor to create a {@code TimeLevelMember} with the specified
	 * {@code id} and {@code ranges}.
	 * 
	 * @param id
	 *            the identifier of the instance
	 * @param ranges
	 *            the ranges defined for {@code this}
	 */
	public TimeLevelMember(final String id,
			final Collection<TimeMemberRange> ranges) {
		this.ranges = new ArrayList<TimeMemberRange>();
		this.id = id;

		if (ranges != null) {
			this.ranges.addAll(ranges);
		}
	}

	/**
	 * Gets the identifier of {@code this}.
	 * 
	 * @return the identifier of {@code this}
	 */
	public String getId() {
		return id;
	}

	/**
	 * Gets the name of {@code this}.
	 * 
	 * @return the name of {@code this}
	 */
	public String getName() {
		return name == null ? id : name;
	}

	/**
	 * Sets the name of {@code this}.
	 * 
	 * @param name
	 *            the name of {@code this}
	 */
	public void setName(final String name) {
		this.name = name;
	}

	/**
	 * Gets an iterator to iterate over the sorted ranges (sorted by start)
	 * specified for the member.
	 * 
	 * @return an iterator to iterate over the ranges specified for the member
	 */
	public Iterator<TimeMemberRange> it() {
		return this.ranges.iterator();
	}

	/**
	 * Gets the ranges of the lowest granularity covered by the member of the
	 * level.
	 * 
	 * @return the ranges of the lowest granularity covered by the member of the
	 *         level
	 */
	public List<TimeMemberRange> getRanges() {
		return Collections.unmodifiableList(this.ranges);
	}

	/**
	 * Determines the size of all ranges of {@code this}.
	 * 
	 * @return the size, i.e. the amount of time-points covered
	 */
	public long size() {
		long size = 0;

		for (final TimeMemberRange range : ranges) {
			size += range.size();
		}

		return size;
	}

	/**
	 * Gets the amount of ranges defined for the member.
	 * 
	 * @return the amount of ranges defined for the member
	 */
	public int sizeOfRanges() {
		return this.ranges.size();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == this) {
			return true;
		} else if (obj == null) {
			return false;
		} else if (obj instanceof TimeLevelMember) {
			final TimeLevelMember tmr = (TimeLevelMember) obj;
			return Objects.equals(tmr.getId(), getId())
					&& Objects.equals(tmr.getRanges(), getRanges());
		} else {
			return false;
		}
	}

	@Override
	public String toString() {
		return getId() + " (" + name + ") " + this.ranges.toString();
	}

	/**
	 * Gets the range specified at the {@code nr} position.
	 * 
	 * @param nr
	 *            the position of the range to retrieve
	 * 
	 * @return the range at the specified position
	 */
	public TimeMemberRange getRange(final int nr) {
		return this.ranges.get(nr);
	}
}
