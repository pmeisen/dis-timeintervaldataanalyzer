package net.meisen.dissertation.model.dimensions.templates;

import java.util.Iterator;

import net.meisen.dissertation.exceptions.TimeDimensionException;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.dissertation.model.dimensions.TimeMemberRange;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

import org.junit.Test;

/**
 * Tests the {@code Minutes} implementation.
 * 
 * @author pmeisen
 * 
 * @see Minutes
 */
public class TestMinutes extends BaseTimeLevelTemplateTest {

	/**
	 * Tests the exception to be thrown, if the dimension is not initialized
	 * correctly.
	 */
	@Test
	public void testUninitalized() {
		final Minutes minutes = new Minutes();

		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(TimeDimensionException.class,
				1002));
		minutes.it(null, "UTC");
	}

	/**
	 * Tests an invalid used granularity (i.e. day-granularity).
	 */
	@Test
	public void testInvalidGranularity() {
		final TidaModel model = m("/net/meisen/dissertation/model/dimensions/templates/testDaysTemplate.xml");

		final Minutes minutes = new Minutes();

		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(TimeDimensionException.class,
				1000));
		minutes.it(model.getIntervalModel(), "UTC");
	}

	/**
	 * Tests the minutes creation of the level.
	 */
	@Test
	public void testMinutesGranularity() {
		Iterator<TimeLevelMember> it;
		final TidaModel model = m("/net/meisen/dissertation/model/dimensions/templates/testMinutesTemplate.xml");

		final Minutes minutes = new Minutes();

		// @formatter:off
		it = minutes.it(model.getIntervalModel(), "America/Los_Angeles");
		assertIterator(
				it,
				new String[] { 
						"20141101_1652_DST",
						"20141102_0101_DST",
						"20141102_0101",
						"20141103_0330"
				},
				new TimeMemberRange[][] { 
						new TimeMemberRange[] { new TimeMemberRange(0, 0) },
						new TimeMemberRange[] { new TimeMemberRange(489, 489) },
						new TimeMemberRange[] { new TimeMemberRange(549, 549) },
						new TimeMemberRange[] { new TimeMemberRange(2138, 2138) }
				});
		// @formatter:on

		// @formatter:off
		it = minutes.it(model.getIntervalModel(), "Europe/Berlin");
		assertIterator(
				it,
				new String[] { 
						"20141102_0052",
						"20141103_1230"
				},
				new TimeMemberRange[][] { 
						new TimeMemberRange[] { new TimeMemberRange(0, 0) },
						new TimeMemberRange[] { new TimeMemberRange(2138, 2138) } 
				});
		// @formatter:on
	}

}
