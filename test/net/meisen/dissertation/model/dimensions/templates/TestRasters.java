package net.meisen.dissertation.model.dimensions.templates;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.util.Iterator;

import net.meisen.dissertation.exceptions.TimeDimensionException;
import net.meisen.dissertation.impl.time.granularity.TimeGranularityFactory;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.dissertation.model.dimensions.TimeMemberRange;
import net.meisen.dissertation.model.time.granularity.Day;
import net.meisen.dissertation.model.time.granularity.Hour;
import net.meisen.dissertation.model.time.granularity.ITimeGranularityFactory;
import net.meisen.dissertation.model.time.granularity.Minute;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

import org.junit.Test;

/**
 * Tests the raster template.
 * 
 * @author pmeisen
 * 
 * @see Rasters
 * 
 */
public class TestRasters extends BaseTimeLevelTemplateTest {
	private ITimeGranularityFactory timeGranularityFactory = new TimeGranularityFactory();

	/**
	 * Tests the exception to be thrown, if the dimension is not initialized
	 * correctly.
	 */
	@Test
	public void testUninitalized() {
		final Rasters rasters = new Rasters(5);

		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(TimeDimensionException.class,
				1002));
		rasters.it(null, "UTC");
	}

	/**
	 * Tests the implementation of the {@code ITimeLevelTemplateFactory}.
	 */
	@Test
	public void testFactory() {
		final Rasters rasters = new Rasters();

		Rasters raster = rasters.createTemplate(timeGranularityFactory,
				"RASTER_HOUR_5");
		assertEquals(5, raster.getBucketSize());
		assertNull(raster.getLevelGranularity());
		assertEquals(Hour.instance(), raster.getGroupGranularity());
		assertEquals("RASTER_HOUR_5", raster.getId());

		raster = rasters.createTemplate(timeGranularityFactory,
				"RASTER_HOUR_HOUR_5");
		assertEquals(5, raster.getBucketSize());
		assertEquals(Hour.instance(), raster.getLevelGranularity());
		assertEquals(Hour.instance(), raster.getGroupGranularity());
		assertEquals("RASTER_HOUR_HOUR_5", raster.getId());

		raster = rasters.createTemplate(timeGranularityFactory, "RASTER_5");
		assertEquals(5, raster.getBucketSize());
		assertNull(raster.getLevelGranularity());
		assertNull(raster.getGroupGranularity());
		assertEquals("RASTER_5", raster.getId());

		raster = rasters.createTemplate(timeGranularityFactory,
				"RASTER_HOUR_MINUTE_5");
		assertEquals(5, raster.getBucketSize());
		assertEquals(Minute.instance(), raster.getLevelGranularity());
		assertEquals(Hour.instance(), raster.getGroupGranularity());
		assertEquals("RASTER_HOUR_MINUTE_5", raster.getId());
	}

	/**
	 * Tests simple rastering on the lowest granularity (i.e. days).
	 */
	@Test
	public void testSimpleRasteringOfDays() {
		Iterator<TimeLevelMember> it;

		final TidaModel model = m("/net/meisen/dissertation/model/dimensions/templates/testDaysTemplate.xml");
		final Rasters rasters = new Rasters(5);

		// @formatter:off
		it = rasters.it(model.getIntervalModel(), "America/Los_Angeles");
		assertIterator(
				it,
				new String[] { 
						"R0-2"
				},
				new TimeMemberRange[][] { 
						new TimeMemberRange[] { new TimeMemberRange(0, 2) }
				});
		// @formatter:on
	}

	/**
	 * Tests simple rastering on the lowest granularity (i.e. minutes).
	 */
	@Test
	public void testSimpleRasteringOfMinutes() {
		Iterator<TimeLevelMember> it;

		final TidaModel model = m("/net/meisen/dissertation/model/dimensions/templates/testMinutesTemplate.xml");
		final Rasters rasters = new Rasters(100);

		// @formatter:off
		it = rasters.it(model.getIntervalModel(), "America/Los_Angeles");
		assertIterator(
				it,
				new String[] { 
						"R0-99",
						"R100-199",
						"R2100-2138"
				},
				new TimeMemberRange[][] { 
						new TimeMemberRange[] { new TimeMemberRange(0, 99) },
						new TimeMemberRange[] { new TimeMemberRange(100, 199) },
						new TimeMemberRange[] { new TimeMemberRange(2100, 2138) }
				});
		// @formatter:on
	}

	/**
	 * Tests the rastering of minutes.
	 */
	@Test
	public void testRasteringOfMinutes() {
		Iterator<TimeLevelMember> it;

		final TidaModel model = m("/net/meisen/dissertation/model/dimensions/templates/testMinutesTemplate.xml");
		final Rasters rasters = new Rasters(100, null, Day.instance());

		// @formatter:off
		it = rasters.it(model.getIntervalModel(), "America/Los_Angeles");
		assertIterator(
				it,
				new String[] { 
						"R20141101_1652_DST_1831_DST",
						"R20141101_1832_DST_2011_DST",
						"R20141101_2332_DST_2359_DST",
						"R20141102_0000_DST_0139_DST",
						"R20141102_0140_DST_0219",
						"R20141103_0140_0319",
						"R20141103_0320_0330"
				},
				new TimeMemberRange[][] { 
						new TimeMemberRange[] { new TimeMemberRange(0, 99) },
						new TimeMemberRange[] { new TimeMemberRange(100, 199) },
						new TimeMemberRange[] { new TimeMemberRange(400, 427) },
						new TimeMemberRange[] { new TimeMemberRange(428, 527) },
						new TimeMemberRange[] { new TimeMemberRange(528, 627) },
						new TimeMemberRange[] { new TimeMemberRange(2028, 2127) },
						new TimeMemberRange[] { new TimeMemberRange(2128, 2138) }
				});
		// @formatter:on

		// @formatter:off
		it = rasters.it(model.getIntervalModel(), "UTC");
		assertIterator(
				it,
				new String[] { 
						"R20141101_2352_2359",
						"R20141102_0000_0139",
						"R20141102_0140_0319",
						"R20141103_0820_0959",
						"R20141103_1000_1130"
				},
				new TimeMemberRange[][] { 
						new TimeMemberRange[] { new TimeMemberRange(0, 7) },
						new TimeMemberRange[] { new TimeMemberRange(8, 107) },
						new TimeMemberRange[] { new TimeMemberRange(108, 207) },
						new TimeMemberRange[] { new TimeMemberRange(1948, 2047) },
						new TimeMemberRange[] { new TimeMemberRange(2048, 2138) }
				});
		// @formatter:on

		// @formatter:off
		it = rasters.it(model.getIntervalModel(), "Europe/Berlin");
		assertIterator(
				it,
				new String[] { 
						"R20141102_0052_0231",
						"R20141102_0232_0411",
						"R20141102_2232_2359",
						"R20141103_0000_0139",
						"R20141103_1140_1230"
				},
				new TimeMemberRange[][] { 
						new TimeMemberRange[] { new TimeMemberRange(0, 99) },
						new TimeMemberRange[] { new TimeMemberRange(100, 199) },
						new TimeMemberRange[] { new TimeMemberRange(1300, 1387) },
						new TimeMemberRange[] { new TimeMemberRange(1388, 1487) },
						new TimeMemberRange[] { new TimeMemberRange(2088, 2138) }
				});
		// @formatter:on
	}

	/**
	 * Tests the rastering by a group for a specific level.
	 */
	@Test
	public void testRasteringOfMinutesWithDayLevelAndGroup() {
		Iterator<TimeLevelMember> it;

		final TidaModel model = m("/net/meisen/dissertation/model/dimensions/templates/testMinutesTemplate.xml");
		final Rasters rasters = new Rasters(2, Day.instance(), Day.instance());

		// @formatter:off
		it = rasters.it(model.getIntervalModel(), "America/Los_Angeles");
		assertIterator(
				it,
				new String[] { 
						"R20141101_20141102",
						"R20141103_20141103"
				},
				new TimeMemberRange[][] { 
						new TimeMemberRange[] { new TimeMemberRange(0, 1927) },
						new TimeMemberRange[] { new TimeMemberRange(1928, 2138) }
				});
		// @formatter:on

		// @formatter:off
		it = rasters.it(model.getIntervalModel(), "UTC");
		assertIterator(
				it,
				new String[] { 
						"R20141101_20141102",
						"R20141103_20141103"
				},
				new TimeMemberRange[][] { 
						new TimeMemberRange[] { new TimeMemberRange(0, 1447) },
						new TimeMemberRange[] { new TimeMemberRange(1448, 2138) }
				});
		// @formatter:on

		// @formatter:off
		it = rasters.it(model.getIntervalModel(), "Europe/Berlin");
		assertIterator(
				it,
				new String[] { 
						"R20141102_20141103"
				},
				new TimeMemberRange[][] { 
						new TimeMemberRange[] { new TimeMemberRange(0, 2138) }
				});
		// @formatter:on
	}

	/**
	 * Tests the rastering by a group for a specific level.
	 */
	@Test
	public void testRasteringOfMinutesWithHourLevelAndGroup() {
		Iterator<TimeLevelMember> it;

		final TidaModel model = m("/net/meisen/dissertation/model/dimensions/templates/testMinutesTemplate.xml");
		final Rasters rasters = new Rasters(8, Hour.instance(), Hour.instance());

		// @formatter:off
		it = rasters.it(model.getIntervalModel(), "America/Los_Angeles");
		assertIterator(
				it,
				new String[] { 
						"R20141101_16_DST_20141101_23_DST",
						"R20141102_00_DST_20141102_06",
						"R20141102_07_20141102_14",
						"R20141102_15_20141102_22",
						"R20141102_23_20141103_03"
				},
				new TimeMemberRange[][] { 
						new TimeMemberRange[] { new TimeMemberRange(0, 427) },
						new TimeMemberRange[] { new TimeMemberRange(428, 907) },
						new TimeMemberRange[] { new TimeMemberRange(908, 1387) },
						new TimeMemberRange[] { new TimeMemberRange(1388, 1867) },
						new TimeMemberRange[] { new TimeMemberRange(1868, 2138) }
				});
		// @formatter:on

		// @formatter:off
		it = rasters.it(model.getIntervalModel(), "UTC");
		assertIterator(
				it,
				new String[] { 
						"R20141101_23_20141102_06",
						"R20141102_07_20141102_14",
						"R20141102_15_20141102_22",
						"R20141102_23_20141103_06",
						"R20141103_07_20141103_11"
				},
				new TimeMemberRange[][] { 
						new TimeMemberRange[] { new TimeMemberRange(0, 427) },
						new TimeMemberRange[] { new TimeMemberRange(428, 907) },
						new TimeMemberRange[] { new TimeMemberRange(908, 1387) },
						new TimeMemberRange[] { new TimeMemberRange(1388, 1867) },
						new TimeMemberRange[] { new TimeMemberRange(1868, 2138) }
				});
		// @formatter:on

		// @formatter:off
		it = rasters.it(model.getIntervalModel(), "Europe/Berlin");
		assertIterator(
				it,
				new String[] { 
						"R20141102_00_20141102_07",
						"R20141102_08_20141102_15",
						"R20141102_16_20141102_23",
						"R20141103_00_20141103_07",
						"R20141103_08_20141103_12"
				},
				new TimeMemberRange[][] { 
						new TimeMemberRange[] { new TimeMemberRange(0, 427) },
						new TimeMemberRange[] { new TimeMemberRange(428, 907) },
						new TimeMemberRange[] { new TimeMemberRange(908, 1387) },
						new TimeMemberRange[] { new TimeMemberRange(1388, 1867) },
						new TimeMemberRange[] { new TimeMemberRange(1868, 2138) }
				});
		// @formatter:on
	}

	/**
	 * Tests the rastering by a group for a specific level.
	 */
	@Test
	public void testRasteringOfMinutesWithHourLevel() {
		Iterator<TimeLevelMember> it;

		final TidaModel model = m("/net/meisen/dissertation/model/dimensions/templates/testMinutesTemplate.xml");
		final Rasters rasters = new Rasters(10, Hour.instance(), Day.instance());

		// @formatter:off
		it = rasters.it(model.getIntervalModel(), "America/Los_Angeles");
		assertIterator(
				it,
				new String[] { 
						"R20141101_16_DST_23_DST",
						"R20141102_00_DST_08",
						"R20141102_09_18",
						"R20141102_19_23",
						"R20141103_00_03"
				},
				new TimeMemberRange[][] { 
						new TimeMemberRange[] { new TimeMemberRange(0, 427) },
						new TimeMemberRange[] { new TimeMemberRange(428, 1027) },
						new TimeMemberRange[] { new TimeMemberRange(1028, 1627) },
						new TimeMemberRange[] { new TimeMemberRange(1628, 1927) },
						new TimeMemberRange[] { new TimeMemberRange(1928, 2138) }
				});
		// @formatter:on

		// @formatter:off
		it = rasters.it(model.getIntervalModel(), "UTC");
		assertIterator(
				it,
				new String[] { 
						"R20141101_23_23",
						"R20141102_00_09",
						"R20141102_10_19",
						"R20141102_20_23",
						"R20141103_00_09",
						"R20141103_10_11"
				},
				new TimeMemberRange[][] { 
						new TimeMemberRange[] { new TimeMemberRange(0, 7) },
						new TimeMemberRange[] { new TimeMemberRange(8, 607) },
						new TimeMemberRange[] { new TimeMemberRange(608, 1207) },
						new TimeMemberRange[] { new TimeMemberRange(1208, 1447) },
						new TimeMemberRange[] { new TimeMemberRange(1448, 2047) },
						new TimeMemberRange[] { new TimeMemberRange(2048, 2138) }
				});
		// @formatter:on

		// @formatter:off
		it = rasters.it(model.getIntervalModel(), "Europe/Berlin");
		assertIterator(
				it,
				new String[] { 
						"R20141102_00_09",
						"R20141102_10_19",
						"R20141102_20_23",
						"R20141103_00_09",
						"R20141103_10_12"
				},
				new TimeMemberRange[][] { 
						new TimeMemberRange[] { new TimeMemberRange(0, 547) },
						new TimeMemberRange[] { new TimeMemberRange(548, 1147) },
						new TimeMemberRange[] { new TimeMemberRange(1148, 1387) },
						new TimeMemberRange[] { new TimeMemberRange(1388, 1987) },
						new TimeMemberRange[] { new TimeMemberRange(1988, 2138) }
				});
		// @formatter:on
	}

	/**
	 * Tests the partly rastering.
	 */
	@Test
	public void testPartlyRastering() {
		Iterator<TimeLevelMember> it;

		final TidaModel model = m("/net/meisen/dissertation/model/dimensions/templates/testMinutesTemplate.xml");
		final Rasters rasters = new Rasters(10, Hour.instance(), Day.instance());

		// @formatter:off
		it = rasters.it(model.getIntervalModel(), 428, 1929, "America/Los_Angeles");
		assertIterator(
				it,
				new String[] { 
						"R20141102_00_DST_08",
						"R20141102_09_18",
						"R20141102_19_23",
						"R20141103_00_03"
				},
				new TimeMemberRange[][] { 
						new TimeMemberRange[] { new TimeMemberRange(428, 1027) },
						new TimeMemberRange[] { new TimeMemberRange(1028, 1627) },
						new TimeMemberRange[] { new TimeMemberRange(1628, 1927) },
						new TimeMemberRange[] { new TimeMemberRange(1928, 2138) }
				});		
		// @formatter:on

		// @formatter:off
		it = rasters.it(model.getIntervalModel(), 428, 1200, "America/Los_Angeles");
		assertIterator(
				it,
				new String[] { 
						"R20141102_00_DST_08",
						"R20141102_09_18"
				},
				new TimeMemberRange[][] { 
						new TimeMemberRange[] { new TimeMemberRange(428, 1027) },
						new TimeMemberRange[] { new TimeMemberRange(1028, 1627) }
				});		
		// @formatter:on
	}
}
