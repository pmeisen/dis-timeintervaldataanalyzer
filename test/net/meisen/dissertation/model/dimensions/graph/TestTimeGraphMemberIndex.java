package net.meisen.dissertation.model.dimensions.graph;

import static org.junit.Assert.assertEquals;

import java.util.Iterator;
import java.util.Set;

import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.dissertation.model.dimensions.TimeMemberRange;

import org.junit.Test;

/**
 * Tests the implementation of the {@code TimeGraphMemberIndex}.
 * 
 * @author pmeisen
 * 
 * @see TimeGraphMemberIndex
 * 
 */
public class TestTimeGraphMemberIndex {

	/**
	 * Tests the indexing of the complete axis.
	 */
	@Test
	public void testCompleteAxis() {
		final TimeGraphMemberIndex idx = new TimeGraphMemberIndex();

		idx.add(new TimeLevelMember("TEST10", 10, 19));
		idx.add(new TimeLevelMember("TEST20", 20, 29));
		idx.add(new TimeLevelMember("TEST30", 30, 39));

		idx.initialize();

		Set<TimeLevelMember> members;
		Iterator<TimeLevelMember> it;

		members = idx.getMembers(5, 9);
		assertEquals(0, members.size());

		members = idx.getMembers(5, 10);
		assertEquals(1, members.size());
		it = members.iterator();
		assertEquals("TEST10", it.next().getId());

		members = idx.getMembers(12, 25);
		assertEquals(2, members.size());
		it = members.iterator();
		assertEquals("TEST10", it.next().getId());
		assertEquals("TEST20", it.next().getId());

		members = idx.getMembers(12, 33);
		assertEquals(3, members.size());
		it = members.iterator();
		assertEquals("TEST10", it.next().getId());
		assertEquals("TEST20", it.next().getId());
		assertEquals("TEST30", it.next().getId());

		members = idx.getMembers(12, 100);
		assertEquals(3, members.size());
		it = members.iterator();
		assertEquals("TEST10", it.next().getId());
		assertEquals("TEST20", it.next().getId());
		assertEquals("TEST30", it.next().getId());

		// check outside
		members = idx.getMembers(40, 40);
		assertEquals(0, members.size());

		// larger start than end
		members = idx.getMembers(5, 3);
		assertEquals(0, members.size());

		// check some single values
		for (int k = 10; k < 40; k += 10) {
			for (int i = k; i < k + 10; i++) {
				members = idx.getMembers(i, i);
				assertEquals(1, members.size());
				it = members.iterator();
				assertEquals("TEST" + k, it.next().getId());
			}
		}
	}

	/**
	 * Tests the indexing of an imcomplete axis.
	 */
	@Test
	public void testIncompleteAxis() {
		final TimeGraphMemberIndex idx = new TimeGraphMemberIndex();

		idx.add(new TimeLevelMember("TEST1", new TimeMemberRange(10, 19),
				new TimeMemberRange(30, 39), new TimeMemberRange(50, 59)));
		idx.add(new TimeLevelMember("TEST2", new TimeMemberRange(20, 29),
				new TimeMemberRange(40, 49), new TimeMemberRange(60, 69)));

		idx.initialize();

		Set<TimeLevelMember> members;
		Iterator<TimeLevelMember> it;

		members = idx.getMembers(40, 40);
		assertEquals(1, members.size());
		it = members.iterator();
		assertEquals("TEST2", it.next().getId());

		members = idx.getMembers(30, 49);
		assertEquals(2, members.size());
		it = members.iterator();
		assertEquals("TEST1", it.next().getId());
		assertEquals("TEST2", it.next().getId());

		members = idx.getMembers(20, 60);
		assertEquals(2, members.size());
		it = members.iterator();
		assertEquals("TEST2", it.next().getId());
		assertEquals("TEST1", it.next().getId());
	}
}
