package net.meisen.dissertation.model.dimensions.graph;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.InputStream;
import java.text.ParseException;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import java.util.Set;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.model.data.DimensionModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.dimensions.IDimension;
import net.meisen.dissertation.model.dimensions.TimeDimension;
import net.meisen.dissertation.model.dimensions.TimeHierarchy;
import net.meisen.dissertation.model.dimensions.TimeLevel;
import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.dissertation.model.handler.TidaDimensionHandler;
import net.meisen.general.genmisc.types.Dates;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the implementation of a {@code TimeGraph}.
 * 
 * @author pmeisen
 * 
 */
public class TestTimeGraph extends LoaderBasedTest {

	/**
	 * The loader to load models.
	 */
	@Autowired
	@Qualifier(DefaultValues.DIMENSIONHANDLER_ID)
	private TidaDimensionHandler dimHandler;

	/**
	 * Helper method to load a dimensional definition from xml.
	 * 
	 * @param name
	 *            the name of the file to be loaded located at
	 *            {@code /net/meisen/dissertation/model/dimensions/config/},
	 *            without any {@code .xml}.
	 * 
	 * @return the loaded dimensions
	 */
	public Map<String, IDimension> loadXml(final String name) {

		// get the resource
		final InputStream is = getClass().getResourceAsStream(
				"/net/meisen/dissertation/model/dimensions/config/" + name
						+ ".xml");
		assertNotNull(is);

		return dimHandler.loadDimensions(is);
	}

	/**
	 * Loads a specific dimension from the specified xml.
	 * 
	 * @param name
	 *            the name of the file to be loaded located at
	 *            {@code /net/meisen/dissertation/model/dimensions/config/},
	 *            without any {@code .xml}.
	 * 
	 * @return the loaded dimension
	 */
	public TimeDimension loadDimension(final String name) {
		final Map<String, IDimension> graphs = loadXml(name);
		assertEquals(1, graphs.size());

		final IDimension dim = graphs.values().iterator().next();
		assertTrue(dim == null ? null : dim.getClass().getSimpleName(),
				dim instanceof TimeDimension);
		return (TimeDimension) dim;
	}

	/**
	 * Loads a {@code TimeGraph} of a specific dimension.
	 * 
	 * @param name
	 *            the name of the file to be loaded located at
	 *            {@code /net/meisen/dissertation/model/dimensions/config/},
	 *            without any {@code .xml}.
	 * @param dimId
	 *            the identifier of the dimension to retrieve the graph for
	 * 
	 * @return the graph
	 */
	public TimeGraph loadFromModel(final String name, final String dimId) {
		final TidaModel model = m("/net/meisen/dissertation/model/dimensions/"
				+ name + ".xml");
		final DimensionModel dimModel = model.getDimensionModel();

		final IDimensionGraph graph = dimModel.getDimension(dimId);
		assertTrue(graph == null ? null : graph.getClass().getSimpleName(),
				graph instanceof TimeGraph);

		return (TimeGraph) graph;
	}

	/**
	 * Load an empty dimension.
	 */
	@Test
	public void testLoadViaEmptyDimensionHandler() {
		final Map<String, IDimension> graphs = loadXml("DIMEMPTY");
		assertEquals(0, graphs.size());
	}

	/**
	 * Load and validate a dimension using the {@code TidaDimensionHandler}.
	 * 
	 * @see TidaDimensionHandler
	 */
	@Test
	public void testLoadViaDimensionHandlerSingleHierarchy() {

		final TimeDimension dim = loadDimension("DIMTIME");
		assertEquals("MyTimeDimensionId", dim.getId());
		assertEquals("My Time Dimension", dim.getName());

		final Collection<TimeHierarchy> hierarchies = dim.getHierarchies();
		assertEquals(1, hierarchies.size());
		final TimeHierarchy hierarchy = hierarchies.iterator().next();
		assertEquals("TIME", hierarchy.getId());
		assertEquals("My Time", hierarchy.getName());
		assertEquals("Europe/Berlin", hierarchy.getTimeZone());

		assertEquals(TimeDimension.ROOT_LEVEL_ID, hierarchy.getRootLevel()
				.getId());
		assertEquals(4, hierarchy.sizeOfLevels());

		// check the levels
		TimeLevel level;
		level = hierarchy.getLevel(0);
		assertEquals("*", level.getId());
		assertEquals("SimpleTime", level.getName());

		level = hierarchy.getLevel(1);
		assertEquals("DAY", level.getId());
		assertEquals("DAYS", level.getTemplateId());

		level = hierarchy.getLevel(2);
		assertEquals("HOUR", level.getId());
		assertEquals("Hours", level.getName());
		assertEquals("HOURS", level.getTemplateId());

		level = hierarchy.getLevel(3);
		assertEquals("MINUTE", level.getId());
		assertEquals("MINUTES", level.getTemplateId());
	}

	/**
	 * Loads a time-dimension with multiple hierarchies.
	 */
	@Test
	public void testLoadViaDimensionHandlerMultipleHierarchies() {
		final TimeDimension dim = loadDimension("DIMMULTIPLEHIERARCHIES");
		assertEquals("MultipleHierarchies", dim.getId());
		assertEquals("MultipleHierarchies", dim.getName());

		final Collection<TimeHierarchy> hierarchies = dim.getHierarchies();
		assertEquals(2, hierarchies.size());
		TimeHierarchy hierarchy = dim.getHierarchy("TIME1");
		assertEquals("TIME1", hierarchy.getId());
		assertEquals("TIME1", hierarchy.getName());
		assertEquals("UTC", hierarchy.getTimeZone());

		hierarchy = dim.getHierarchy("TIME2");
		assertEquals("TIME2", hierarchy.getId());
		assertEquals("TIME2", hierarchy.getName());
		assertEquals("America/Chicago", hierarchy.getTimeZone());
	}

	/**
	 * Does some really simple testing of the iteration over the level, using a
	 * minute-granularity.
	 * 
	 * @throws ParseException
	 *             exception if a date is wrong.
	 */
	@Test
	public void testMinutesHierarchies() throws ParseException {
		final TimeGraph graph = loadFromModel("testTimeModelMinutes",
				"DIM_TIME");

		// check the lazy settings
		assertTrue(graph.isLazy("TIME_PURE_RASTER_QUARTER", "MINUTE10"));
		assertFalse(graph.isLazy("TIME_PURE_RASTER_QUARTER", "MINUTE5"));

		Date start, end;
		Set<TimeLevelMember> members;

		// test some retrieval
		start = Dates.parseDate("02.02.2014 00:00", "dd.MM.yyyy HH:mm",
				"America/Chicago");
		end = Dates.parseDate("02.02.2014 23:59", "dd.MM.yyyy HH:mm",
				"America/Chicago");
		members = graph.getMembers("TIME_PURE_RASTER_WEEK", "MINUTE30", start,
				end);
		assertEquals(48, members.size());
		int i = 46440;
		for (final TimeLevelMember member : members) {
			assertEquals(1, member.getRanges().size());
			assertEquals(i, member.getRange(0).getStart());
			i += 30;
		}

		start = Dates.parseDate("09.03.2014 00:00", "dd.MM.yyyy HH:mm",
				"America/Chicago");
		end = Dates.parseDate("09.03.2014 23:59", "dd.MM.yyyy HH:mm",
				"America/Chicago");
		members = graph.getMembers("TIME_PURE_RASTER_WEEK", "MINUTE5", start,
				end);
		assertEquals(276, members.size());
		i = 96840;
		for (final TimeLevelMember member : members) {
			assertEquals(1, member.getRanges().size());
			assertEquals(i, member.getRange(0).getStart());
			i += 5;
		}
	}

	/**
	 * Does some really simple testing of the iteration over the level, using a
	 * second-granularity.
	 * 
	 * @throws ParseException
	 *             exception if a date is wrong.
	 */
	@Test
	public void testSecondsHierarchies() throws ParseException {
		final TimeGraph graph = loadFromModel("testTimeModelSeconds",
				"DIM_TIME");

		Date start, end;
		Set<TimeLevelMember> members;

		// test some retrieval
		start = Dates.parseDate("02.02.2014 00:00", "dd.MM.yyyy HH:mm",
				"America/Chicago");
		end = Dates.parseDate("02.02.2014 23:59", "dd.MM.yyyy HH:mm",
				"America/Chicago");
		members = graph.getMembers("TIME_PURE_RASTER_WEEK", "MINUTE30", start,
				end);
		assertEquals(48, members.size());
		int i = 2786400;
		for (final TimeLevelMember member : members) {
			assertEquals(1, member.getRanges().size());
			assertEquals(i, member.getRange(0).getStart());
			i += 30 * 60;
		}

		start = Dates.parseDate("09.03.2014 00:00", "dd.MM.yyyy HH:mm",
				"America/Chicago");
		end = Dates.parseDate("09.03.2014 23:59", "dd.MM.yyyy HH:mm",
				"America/Chicago");
		members = graph.getMembers("TIME_PURE_RASTER_WEEK", "MINUTE5", start,
				end);
		assertEquals(276, members.size());
		i = 5810400;
		for (final TimeLevelMember member : members) {
			assertEquals(1, member.getRanges().size());
			assertEquals(i, member.getRange(0).getStart());
			i += 5 * 60;
		}
	}
}
