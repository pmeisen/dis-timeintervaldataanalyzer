package net.meisen.dissertation.model.dimensions.templates;

import java.util.Iterator;

import net.meisen.dissertation.exceptions.TimeDimensionException;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.dissertation.model.dimensions.TimeMemberRange;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

import org.junit.Test;

/**
 * Tests the implementation of the days-level.
 * 
 * @author pmeisen
 * 
 */
public class TestDays extends BaseTimeLevelTemplateTest {

	/**
	 * Tests the exception to be thrown, if the dimension is not initialized
	 * correctly.
	 */
	@Test
	public void testUninitalized() {
		final Days days = new Days();

		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(TimeDimensionException.class,
				1002));
		days.it(null, "UTC");
	}

	/**
	 * Tests an invalid used granularity (i.e. year-granularity).
	 */
	@Test
	public void testInvalidGranularity() {
		final TidaModel model = m("/net/meisen/dissertation/model/dimensions/templates/testYearsTemplate.xml");

		final Days days = new Days();

		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(TimeDimensionException.class,
				1000));
		days.it(model.getIntervalModel(), "UTC");
	}

	/**
	 * Tests the calculation on a day-granularity time-level. The day
	 * granularity does not allow the usage of time-zones.
	 */
	@Test
	public void testDaysGranularity() {
		Iterator<TimeLevelMember> it;
		final TidaModel model = m("/net/meisen/dissertation/model/dimensions/templates/testDaysTemplate.xml");

		final Days days = new Days();

		// @formatter:off
		it = days.it(model.getIntervalModel(), "America/Los_Angeles");
		assertIterator(
				it,
				new String[] { 
						"20141101",
						"20141102",
						"20141103"
				},
				new TimeMemberRange[][] { 
						new TimeMemberRange[] { new TimeMemberRange(0, 0) },
						new TimeMemberRange[] { new TimeMemberRange(1, 1) },
						new TimeMemberRange[] { new TimeMemberRange(2, 2) } 
				});
		// @formatter:on

		// @formatter:off
		it = days.it(model.getIntervalModel(), "Europe/Berlin");
		assertIterator(
				it,
				new String[] { 
						"20141101",
						"20141102",
						"20141103"
				},
				new TimeMemberRange[][] { 
						new TimeMemberRange[] { new TimeMemberRange(0, 0) },
						new TimeMemberRange[] { new TimeMemberRange(1, 1) },
						new TimeMemberRange[] { new TimeMemberRange(2, 2) } 
				});
		// @formatter:on
	}

	/**
	 * Tests the calculation on a minute-granularity time-level. The minute
	 * granularity allows the usage of time-zones, because the information is
	 * available.
	 */
	@Test
	public void testMinutesGranularity() {
		Iterator<TimeLevelMember> it;
		final TidaModel model = m("/net/meisen/dissertation/model/dimensions/templates/testMinutesTemplate.xml");

		final Days days = new Days();

		// @formatter:off
		it = days.it(model.getIntervalModel(), "America/Los_Angeles");
		assertIterator(
				it,
				new String[] { 
						"20141101",
						"20141102",
						"20141103"
				},
				new TimeMemberRange[][] { 
						new TimeMemberRange[] { new TimeMemberRange(0, 427) },
						new TimeMemberRange[] { new TimeMemberRange(428, 1927) },
						new TimeMemberRange[] { new TimeMemberRange(1928, 2138) } 
				});
		// @formatter:on

		// @formatter:off
		it = days.it(model.getIntervalModel(), "Europe/Berlin");
		assertIterator(
				it,
				new String[] { 
						"20141102",
						"20141103"
				},
				new TimeMemberRange[][] { 
						new TimeMemberRange[] { new TimeMemberRange(0, 1387) },
						new TimeMemberRange[] { new TimeMemberRange(1388, 2138) } 
				});
		// @formatter:on
	}
}
