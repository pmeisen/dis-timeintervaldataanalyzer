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

	public TimeLevelMember(final String id, final long start, final long end) {
		this(id, new TimeMemberRange(start, end));
	}

	public TimeLevelMember(final String id, final TimeMemberRange... ranges) {
		this(id, ranges == null ? null : Arrays.asList(ranges));
	}

	public TimeLevelMember(final String id,
			final Collection<TimeMemberRange> ranges) {
		this.ranges = new ArrayList<TimeMemberRange>();
		this.id = id;

		if (ranges != null) {
			this.ranges.addAll(ranges);
		}
	}

	public String getId() {
		return id;
	}

	public String getName() {
		return name == null ? id : name;
	}

	public void setName(String name) {
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

	public List<TimeMemberRange> getRanges() {
		return Collections.unmodifiableList(this.ranges);
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
	
	public TimeMemberRange getRange(final int nr) {
		return this.ranges.get(nr);
	}
}
