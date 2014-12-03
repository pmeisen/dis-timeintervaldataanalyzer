package net.meisen.dissertation.model.dimensions;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.InputStream;
import java.util.Collection;
import java.util.Map;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.handler.TidaDimensionHandler;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the implementation of the {@link TidaDimensionHandler}.
 * 
 * @author pmeisen
 * 
 */
@RunWith(JUnitConfigurationRunner.class)
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
public class TestDimensionHandler {

	@Autowired
	@Qualifier(DefaultValues.DIMENSIONHANDLER_ID)
	private TidaDimensionHandler dimensionHandler;

	/**
	 * Makes sure that everything was wired correctly.
	 */
	@Before
	public void checkWiring() {
		assertNotNull(dimensionHandler);
	}

	/**
	 * Tests an empty definition file.
	 */
	@Test
	public void testEmpty() {
		final InputStream stream = getClass()
				.getResourceAsStream(
						"/net/meisen/dissertation/model/dimensions/config/DIMEMPTY.xml");
		final Map<String, IDimension> dimensions = dimensionHandler
				.loadDimensions(stream);

		assertEquals(0, dimensions.size());
	}

	/**
	 * Tests a simple definition covering several use-cases.
	 */
	@Test
	public void testSimpleDimension() {
		final InputStream stream = getClass().getResourceAsStream(
				"/net/meisen/dissertation/model/dimensions/config/DIMLOC.xml");
		final Map<String, IDimension> dims = dimensionHandler
				.loadDimensions(stream);

		// make sure we have the right dimension
		assertEquals(1, dims.size());
		final IDimension dim = dims.values().iterator().next();
		assertTrue(dim instanceof DescriptorDimension);

		// check the dimensions attributes
		final DescriptorDimension descDim = (DescriptorDimension) dim;
		assertEquals("DIMLOC", descDim.getId());
		assertEquals("LOC", descDim.getDescriptorModelId());
		assertEquals("DIMLOC", descDim.getName());

		// check the hierarchies
		DescriptorHierarchy hierarchy;
		Collection<DescriptorMember> members;
		assertEquals(2, descDim.getHierarchies().size());
		hierarchy = descDim.getHierarchy("LOC");
		assertEquals("LOC", hierarchy.getId());
		assertEquals("LOC", hierarchy.getName());
		assertEquals(3, hierarchy.getLevels().size());
		for (final DescriptorLevel level : hierarchy.getLevels()) {
			members = hierarchy.getMembers(level.getId());

			// test the levels
			if (level.getId().equals("*")) {
				assertEquals("All Locations", level.getName());
				assertEquals(1, members.size());
				assertEquals("*", members.iterator().next().getId());
			} else if (level.getId().equals("HOTEL")) {
				assertEquals(level.getId(), level.getName());
				assertEquals(4, members.size());

				// test the members of the level
				for (final DescriptorMember member : members) {
					if (member.getId().equals("DREAM")) {
						assertEquals("Traum", member.getName());
						assertNull(member.getPattern());
						assertEquals(1, member.getRollUpTo().size());
						assertEquals("*", member.getRollUpTo().iterator()
								.next().getId());
					} else if (member.getId().equals("STAR")) {
						assertNull(member.getPattern());
						assertEquals(1, member.getRollUpTo().size());
						assertEquals("*", member.getRollUpTo().iterator()
								.next().getId());
					} else if (member.getId().equals("ADV")) {
						assertEquals("TENT\\d", member.getPattern());
						assertEquals(1, member.getRollUpTo().size());
						assertEquals("*", member.getRollUpTo().iterator()
								.next().getId());
					} else if (member.getId().equals("TENT")) {
						assertEquals("\\QTENT\\E", member.getPattern());
						assertEquals(1, member.getRollUpTo().size());
						assertEquals("*", member.getRollUpTo().iterator()
								.next().getId());
					} else {
						fail("Invalid member found '" + member.getId() + "'.");
					}
				}
			} else if (level.getId().equals("ROOMS")) {
				assertEquals("Räume", level.getName());
				assertEquals(3, members.size());

				// test the members of the level
				for (final DescriptorMember member : members) {
					if (member.getId().equals("POSF")) {
						assertEquals("POS F\\d", member.getPattern());
						assertEquals(1, member.getRollUpTo().size());
						assertEquals("DREAM", member.getRollUpTo().iterator()
								.next().getId());
					} else if (member.getId().equals("POSG")) {
						assertEquals("POS G\\d", member.getPattern());
						assertEquals(2, member.getRollUpTo().size());

						// check the rollUpTo-Members
						for (final DescriptorMember m : member.getRollUpTo()) {
							if (m.getId().equals("DREAM")
									|| m.getId().equals("STAR")) {
								// found the right one
							} else {
								fail("Invalid rollUpTo-member found '"
										+ m.getId() + "'.");
							}
						}
					} else if (member.getId().equals("POSA")) {
						assertEquals("POS A\\d", member.getPattern());
						assertEquals(1, member.getRollUpTo().size());
						assertEquals("STAR", member.getRollUpTo().iterator()
								.next().getId());
					} else {
						fail("Invalid member found '" + member.getId() + "'.");
					}
				}
			} else {
				fail("Invalid level found '" + level.getId() + "'.");
			}
		}

		hierarchy = descDim.getHierarchy("GEO");
		assertEquals("GEO", hierarchy.getId());
		assertEquals("Geo", hierarchy.getName());
		assertEquals(2, hierarchy.getLevels().size());
	}
}
