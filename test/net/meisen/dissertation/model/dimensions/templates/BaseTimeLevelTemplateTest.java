package net.meisen.dissertation.model.dimensions.templates;

import static org.junit.Assert.assertEquals;

import java.util.Iterator;

import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.dissertation.model.dimensions.TimeMemberRange;

/**
 * Base implementation for test of {@code BaseTimeLevelTemplate} instances.
 * 
 * @author pmeisen
 * 
 * @see BaseTimeLevelTemplate
 */
public class BaseTimeLevelTemplateTest extends LoaderBasedTest {

	/**
	 * Helper method to test an iterator.
	 * 
	 * @param it
	 *            the iterator to be tested
	 * @param ids
	 *            the identifiers to be expected
	 * @param ranges
	 *            the ranges to be expected for the different identifiers
	 */
	protected void assertIterator(final Iterator<TimeLevelMember> it,
			final String[] ids, final TimeMemberRange[][] ranges) {

		int k = 0;
		while (it.hasNext()) {
			final TimeLevelMember member = it.next();

			if (k >= ids.length) {
				break;
			} else if (!ids[k].equals(member.getId())) {
				continue;
			}

			int i = 0;
			final Iterator<TimeMemberRange> memberIt = member.it();
			while (memberIt.hasNext()) {
				final TimeMemberRange range = memberIt.next();
				assertEquals(ranges[k][i], range);
				i++;
			}
			k++;
		}

		assertEquals(k, ids.length);
	}
}
