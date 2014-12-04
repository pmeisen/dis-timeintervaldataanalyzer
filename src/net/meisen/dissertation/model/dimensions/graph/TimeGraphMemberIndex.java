package net.meisen.dissertation.model.dimensions.graph;

import gnu.trove.list.array.TLongArrayList;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import net.meisen.dissertation.exceptions.DimensionModelException;
import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.dissertation.model.dimensions.TimeMemberRange;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * An index used to retrieve the members of a specific level of a
 * time-dimension.
 * 
 * @author pmeisen
 * 
 */
public class TimeGraphMemberIndex {
	private final Map<String, TimeLevelMember> members;

	private TLongArrayList list;
	private List<TimeLevelMember> positionedMembers;
	private long minValue;
	private long maxValue;
	private boolean initialized;

	/**
	 * Default constructor.
	 */
	public TimeGraphMemberIndex() {
		this.members = new HashMap<String, TimeLevelMember>();

		this.list = null;
		this.positionedMembers = null;
		this.maxValue = -1;
		this.initialized = false;
	}

	/**
	 * Adds the member to the index and resets initialization, i.e.
	 * {@link #initialize()} must be recalled.
	 * 
	 * @param member
	 *            the member to be added
	 */
	public void add(final TimeLevelMember member) {
		this.initialized = false;
		this.members.put(member.getId(), member);
	}

	/**
	 * Gets the members within the specified range.
	 * 
	 * @param start
	 *            the start value of the range to retrieve the members for
	 * @param end
	 *            the end value of the range to retrieve the members for
	 * 
	 * @return the members within the range
	 */
	public Set<TimeLevelMember> getMembers(final long start, final long end) {
		if (!isInitialized()) {
			throw new ForwardedRuntimeException(DimensionModelException.class,
					1012);
		} else if (end < start) {
			return Collections.<TimeLevelMember> emptySet();
		} else {
			int posStart = getPosition(start, true);
			int posEnd = getPosition(end, false);

			// if the start was not within, it's set to the start value
			if (posStart < 0 && posEnd < 0) {
				return Collections.<TimeLevelMember> emptySet();
			} else if (posStart < 0) {
				posStart = posEnd;
			} else if (posEnd < 0) {
				posEnd = list.size() - 1;
			}

			return new LinkedHashSet<TimeLevelMember>(
					positionedMembers.subList(posStart, posEnd + 1));
		}
	}

	/**
	 * Gets the position of the value
	 * 
	 * @param value
	 *            the value to be found
	 * @param start
	 *            {@code true} if the value is start-value, otherwise
	 *            {@code false}
	 * 
	 * @return the position of the value
	 */
	protected int getPosition(final long value, final boolean start) {
		if (value < minValue || value > maxValue) {
			return -1;
		}
		final int found = list.binarySearch(value);

		// get the position
		int pos;
		if (found < 0) {
			pos = -1 * found - 2;
		} else {
			pos = found;
		}

		return pos;
	}

	/**
	 * Validates if the index is initialized.
	 * 
	 * @return {@code true} if the index is initialized, otherwise {@code false}
	 */
	public boolean isInitialized() {
		return initialized;
	}

	/**
	 * Initializes the index.
	 */
	public void initialize() {

		// if we are initialized nothing to do
		if (this.initialized) {
			return;
		}

		// initialize the maps
		final int size = this.members.size();
		this.list = new TLongArrayList(size);
		this.positionedMembers = new ArrayList<TimeLevelMember>(size);

		// add the members
		final SortedMap<TimeMemberRange, TimeLevelMember> ranges = new TreeMap<TimeMemberRange, TimeLevelMember>(
				new Comparator<TimeMemberRange>() {

					@Override
					public int compare(final TimeMemberRange tr1,
							final TimeMemberRange tr2) {
						final long start1 = tr1.getStart();
						final long start2 = tr2.getStart();

						if (start1 < start2) {
							return -1;
						} else if (start1 > start2) {
							return 1;
						} else {
							return 0;
						}
					}
				});

		for (final TimeLevelMember member : this.members.values()) {
			final Iterator<TimeMemberRange> it = member.it();

			while (it.hasNext()) {
				final TimeMemberRange range = it.next();
				ranges.put(range, member);
			}
		}

		for (final Entry<TimeMemberRange, TimeLevelMember> e : ranges
				.entrySet()) {
			final long start = e.getKey().getStart();

			this.list.add(start);
			this.positionedMembers.add(e.getValue());
		}
		this.minValue = ranges.firstKey().getStart();
		this.maxValue = ranges.lastKey().getEnd();

		this.initialized = true;
	}
}
