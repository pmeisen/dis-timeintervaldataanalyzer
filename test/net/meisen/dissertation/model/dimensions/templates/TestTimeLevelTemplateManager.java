package net.meisen.dissertation.model.dimensions.templates;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.dimensions.templates.mock.MockTimeLevelTemplate;
import net.meisen.dissertation.model.time.granularity.Day;
import net.meisen.dissertation.model.time.granularity.Hour;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * Test the manager for {@code TimeTemplate} instances.
 * 
 * @author pmeisen
 * 
 */
public class TestTimeLevelTemplateManager {

	/**
	 * Tests the loading of a mock template.
	 * 
	 * @author pmeisen
	 * 
	 */
	@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/model/dimensions/templates/testConfigWithMockTemplate.xml")
	public static class TestMockLoading extends LoaderBasedTest {

		/**
		 * Tests if the mock was loaded
		 */
		@Test
		public void testMock() {
			final TidaModel model = m("/net/meisen/dissertation/config/simplestModel.xml");
			final ITimeLevelTemplate template = model.getDimensionModel()
					.getTimeLevelTemplate("MOCK");

			assertNotNull(template);
			assertTrue(template.getClass().equals(MockTimeLevelTemplate.class));
		}

		/**
		 * Tests the usage of a factory, i.e. using the Rasters implementation.
		 */
		@Test
		public void testRaster() {
			final TidaModel model = m("/net/meisen/dissertation/config/simplestModel.xml");
			final ITimeLevelTemplate template = model.getDimensionModel()
					.getTimeLevelTemplate("RASTER_HOUR_DAY_5");
			assertTrue(template instanceof Rasters);
			assertEquals("RASTER_HOUR_DAY_5", template.getId());

			final Rasters rTemplate = (Rasters) template;
			assertEquals(Day.instance(), rTemplate.getLevelGranularity());
			assertEquals(Hour.instance(), rTemplate.getGroupGranularity());

			assertTrue(template == model.getDimensionModel()
					.getTimeLevelTemplate("RASTER_HOUR_DAY_5"));
		}
	}

	/**
	 * Suite to test the manager
	 * 
	 * @author pmeisen
	 * 
	 */
	@RunWith(Suite.class)
	@Suite.SuiteClasses({ TestMockLoading.class })
	public static class TestTimeLevelTemplateManagerSuite {
		// just the suite with all the tests defined here
	}
}
