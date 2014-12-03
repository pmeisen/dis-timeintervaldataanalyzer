package net.meisen.dissertation.model.dimensions.graph;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Arrays;
import java.util.Set;

import net.meisen.dissertation.exceptions.DescriptorDimensionException;
import net.meisen.dissertation.model.dimensions.BaseDescriptorDimensionTest;
import net.meisen.dissertation.model.dimensions.DescriptorDimension;
import net.meisen.dissertation.model.dimensions.DescriptorHierarchy;
import net.meisen.dissertation.model.dimensions.DescriptorMember;
import net.meisen.dissertation.model.dimensions.graph.DescriptorGraph;
import net.meisen.dissertation.model.dimensions.graph.DescriptorGraphLevel;
import net.meisen.dissertation.model.dimensions.graph.DescriptorGraphNode;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

import org.junit.Test;

/**
 * Tests the generated {@code DescriptorGraph}.
 * 
 * @author pmeisen
 * 
 */
public class TestDescriptorGraph extends BaseDescriptorDimensionTest {

	/**
	 * Tests the nodes and levels of a location-based dimension.
	 */
	@Test
	public void testNodesAndLevelsOfLocation() {
		DescriptorGraphNode descriptorGraphNode;
		DescriptorGraphLevel descriptorGraphLevel;

		final DescriptorDimension dim = createLocationDimension();

		final DescriptorGraph graph = new DescriptorGraph();
		graph.create(dim);

		assertEquals(9, graph.getNodes().size());
		assertEquals(3, graph.getLevels().size());

		/*
		 * Check the nodes.
		 */
		descriptorGraphNode = graph.getNode("HIERARCHY_LOCATION",
				DescriptorDimension.ROOT_LEVEL_ID);
		assertNotNull(descriptorGraphNode);
		assertFalse(descriptorGraphNode.isSource());
		assertTrue(descriptorGraphNode.isSink());
		assertEquals(0, descriptorGraphNode.getMinDistance());
		assertEquals(0, descriptorGraphNode.getMaxDistance());

		descriptorGraphNode = graph.getNode("HIERARCHY_LOCATION", "STADT_DE");
		assertNotNull(descriptorGraphNode);
		assertTrue(descriptorGraphNode.isSource());
		assertFalse(descriptorGraphNode.isSink());
		assertEquals(2, descriptorGraphNode.getMinDistance());
		assertEquals(2, descriptorGraphNode.getMaxDistance());

		descriptorGraphNode = graph.getNode("HIERARCHY_LOCATION", "LAND_VAT");
		assertNotNull(descriptorGraphNode);
		assertTrue(descriptorGraphNode.getChildren().toString(),
				descriptorGraphNode.isSource());
		assertFalse(descriptorGraphNode.getParents().toString(),
				descriptorGraphNode.isSink());
		assertEquals(1, descriptorGraphNode.getMinDistance());
		assertEquals(1, descriptorGraphNode.getMaxDistance());

		descriptorGraphNode = graph.getNode("HIERARCHY_LOCATION", "LAND_FR");
		assertNotNull(descriptorGraphNode);
		assertTrue(descriptorGraphNode.getChildren().toString(),
				descriptorGraphNode.isSource());
		assertFalse(descriptorGraphNode.getParents().toString(),
				descriptorGraphNode.isSink());
		assertEquals(1, descriptorGraphNode.getMinDistance());
		assertEquals(1, descriptorGraphNode.getMaxDistance());

		descriptorGraphNode = graph.getNode("HIERARCHY_LOCATION", "LAND_DE");
		assertNotNull(descriptorGraphNode);
		assertFalse(descriptorGraphNode.getChildren().toString(),
				descriptorGraphNode.isSource());
		assertFalse(descriptorGraphNode.getParents().toString(),
				descriptorGraphNode.isSink());
		assertEquals(1, descriptorGraphNode.getMinDistance());
		assertEquals(1, descriptorGraphNode.getMaxDistance());

		descriptorGraphNode = graph.getNode("HIERARCHY_LOCATION", "USA");
		assertNotNull(descriptorGraphNode);
		assertFalse(descriptorGraphNode.getChildren().toString(),
				descriptorGraphNode.isSource());
		assertFalse(descriptorGraphNode.getParents().toString(),
				descriptorGraphNode.isSink());
		assertEquals(1, descriptorGraphNode.getMinDistance());
		assertEquals(1, descriptorGraphNode.getMaxDistance());

		/*
		 * Check the levels.
		 */
		descriptorGraphLevel = graph.getLevel("HIERARCHY_LOCATION",
				DescriptorDimension.ROOT_LEVEL_ID);
		assertNotNull(descriptorGraphLevel);
		assertEquals(1, descriptorGraphLevel.getDescriptorLevels().size());
		assertEquals("*", descriptorGraphLevel.getDescriptorLevelId());
		assertEquals(1, descriptorGraphLevel.getNodes().size());
		assertTrue(descriptorGraphLevel.isShared());
		assertTrue(descriptorGraphLevel.isRoot());
		assertFalse(descriptorGraphLevel.isEmpty());
		assertFalse(descriptorGraphLevel.isLeaf());
		assertEquals(0, descriptorGraphLevel.getMaxDistance());
		assertEquals(0, descriptorGraphLevel.getMinDistance());

		descriptorGraphLevel = graph.getLevel("HIERARCHY_LOCATION", "Land");
		assertNotNull(descriptorGraphLevel);
		assertEquals(1, descriptorGraphLevel.getDescriptorLevels().size());
		assertEquals("Land", descriptorGraphLevel.getDescriptorLevelId());
		assertEquals(5, descriptorGraphLevel.getNodes().size());
		assertFalse(descriptorGraphLevel.isShared());
		assertFalse(descriptorGraphLevel.isRoot());
		assertFalse(descriptorGraphLevel.isEmpty());
		assertFalse(descriptorGraphLevel.isLeaf());
		assertEquals(1, descriptorGraphLevel.getMaxDistance());
		assertEquals(1, descriptorGraphLevel.getMinDistance());

		descriptorGraphLevel = graph.getLevel("HIERARCHY_LOCATION", "Stadt");
		assertNotNull(descriptorGraphLevel);
		assertEquals(1, descriptorGraphLevel.getDescriptorLevels().size());
		assertEquals("Stadt", descriptorGraphLevel.getDescriptorLevelId());
		assertEquals(3, descriptorGraphLevel.getNodes().size());
		assertFalse(descriptorGraphLevel.isShared());
		assertFalse(descriptorGraphLevel.isRoot());
		assertFalse(descriptorGraphLevel.isEmpty());
		assertTrue(descriptorGraphLevel.isLeaf());
		assertEquals(2, descriptorGraphLevel.getMaxDistance());
		assertEquals(2, descriptorGraphLevel.getMinDistance());

		descriptorGraphLevel = graph.getLevel("HIERARCHY_LOCATION",
				DescriptorDimension.UNASSIGNED_LEVEL_ID);
		assertNull(descriptorGraphLevel);
	}

	/**
	 * Tests the nodes and levels of a book-based dimension.
	 */
	@Test
	public void testNodesAndLevelsOfBook() {
		DescriptorGraphNode descriptorGraphNode;
		DescriptorGraphLevel descriptorGraphLevel;

		final DescriptorDimension dim = createBookDimension();

		final DescriptorGraph graph = new DescriptorGraph();
		graph.create(dim);

		assertEquals(8, graph.getNodes().size());
		assertEquals(5, graph.getLevels().size());

		/*
		 * Check the nodes.
		 */
		descriptorGraphNode = graph.getNode("HIERARCHY_STRUCTURE",
				DescriptorDimension.ROOT_LEVEL_ID);
		assertNotNull(descriptorGraphNode);
		assertFalse(descriptorGraphNode.isSource());
		assertTrue(descriptorGraphNode.isSink());
		assertEquals(0, descriptorGraphNode.getMinDistance());
		assertEquals(0, descriptorGraphNode.getMaxDistance());

		descriptorGraphNode = graph.getNode("HIERARCHY_STRUCTURE", "0");
		assertNotNull(descriptorGraphNode);
		assertFalse(descriptorGraphNode.getChildren().toString(),
				descriptorGraphNode.isSource());
		assertFalse(descriptorGraphNode.getParents().toString(),
				descriptorGraphNode.isSink());
		assertEquals(1, descriptorGraphNode.getMinDistance());
		assertEquals(1, descriptorGraphNode.getMaxDistance());

		/*
		 * Check the levels.
		 */
		descriptorGraphLevel = graph.getLevel("HIERARCHY_STRUCTURE", "Meta");
		assertNotNull(descriptorGraphLevel);
		assertEquals(1, descriptorGraphLevel.getDescriptorLevels().size());
		assertEquals("Meta", descriptorGraphLevel.getDescriptorLevelId());
		assertEquals(2, descriptorGraphLevel.getNodes().size());
		assertFalse(descriptorGraphLevel.isShared());
		assertFalse(descriptorGraphLevel.isRoot());
		assertFalse(descriptorGraphLevel.isEmpty());
		assertTrue(descriptorGraphLevel.isLeaf());
		assertEquals(2, descriptorGraphLevel.getMaxDistance());
		assertEquals(1, descriptorGraphLevel.getMinDistance());
	}

	/**
	 * Tests the retrieval of members for a specific hierarchy and level.
	 */
	@Test
	public void testMemberRetrieval() {
		DescriptorGraphLevel descriptorGraphLevel;
		Set<DescriptorMember> members;

		final DescriptorDimension dim = createBookDimension();
		final DescriptorGraph graph = new DescriptorGraph();
		graph.create(dim);

		// get an unknown dimension
		assertNull(graph.getLevel("HIERARCHY_STRUCTURE2", "Content"));

		// check some specific level and the members
		descriptorGraphLevel = graph.getLevel("HIERARCHY_STRUCTURE", "Content");
		members = descriptorGraphLevel.getMembers("HIERARCHY_STRUCTURE");
		assertEquals(2, members.size());

		descriptorGraphLevel = graph
				.getLevel("HIERARCHY_STRUCTURE", "Preamble");
		members = descriptorGraphLevel.getMembers("HIERARCHY_STRUCTURE");
		assertEquals(1, members.size());
		assertEquals("0", members.iterator().next().getId());

		// get the members of an undefined hierarchy
		members = descriptorGraphLevel.getMembers("HIERARCHY_STRUCTURE2");
		assertEquals(0, members.size());
	}

	/**
	 * Tests the retrieval of leaf-members (i.e. sources) from a specific member
	 * within the graph.
	 */
	@Test
	public void testLeafMemberRetrieval() {
		DescriptorGraphLevel descriptorGraphLevel;
		Set<DescriptorMember> members;

		final DescriptorDimension dim = createBookDimension();
		final DescriptorGraph graph = new DescriptorGraph();
		graph.create(dim);

		descriptorGraphLevel = graph.getLevel("HIERARCHY_STRUCTURE", "Content");
		members = descriptorGraphLevel.getLeafMembers("HIERARCHY_STRUCTURE2",
				"NOTAVAILABLE");
		assertEquals(0, members.size());

		members = descriptorGraphLevel.getLeafMembers("HIERARCHY_STRUCTURE",
				"NOTAVAILABLE");
		assertEquals(0, members.size());

		members = descriptorGraphLevel.getLeafMembers("HIERARCHY_STRUCTURE",
				"2");
		assertEquals(1, members.size());
		assertEquals("2", members.iterator().next().getId());

		members = descriptorGraphLevel.getLeafMembers("HIERARCHY_STRUCTURE",
				"1");
		assertEquals(3, members.size());
		for (final DescriptorMember member : members) {
			if (member.getId().equals("TOC")) {
				// good we found it
			} else if (member.getId().equals("A0-9")) {
				// good we found it
			} else if (member.getId().equals("B0-9")) {
				// good we found it
			} else {
				fail("Unexpected member found: " + member.getId());
			}
		}

		descriptorGraphLevel = graph
				.getLevel("HIERARCHY_STRUCTURE", "Preamble");
		members = descriptorGraphLevel.getLeafMembers("HIERARCHY_STRUCTURE",
				"0");
		assertEquals(1, members.size());
		assertEquals("TOC", members.iterator().next().getId());

		descriptorGraphLevel = graph.getLevel("HIERARCHY_STRUCTURE", "*");
		members = descriptorGraphLevel.getLeafMembers("HIERARCHY_STRUCTURE",
				"*");
		assertEquals(5, members.size());
	}

	/**
	 * Tests the detection of an empty dimension.
	 */
	@Test
	public void testInvalidEmptyDimension() {
		final DescriptorDimension dim = createInvalidEmptyDimension();

		// no root available
		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(
				DescriptorDimensionException.class, 1009));

		final DescriptorGraph graph = new DescriptorGraph();
		graph.create(dim);
	}

	/**
	 * Tests the detection of a multiple root dimension.
	 */
	@Test
	public void testInvalidMultipleRootDimension() {

		// multiple root members
		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(
				DescriptorDimensionException.class, 1003));

		createInvalidMultipleRootDimension();
	}

	/**
	 * Tests the detection of a cycle.
	 */
	@Test
	public void testInvalidCycleDimension() {
		final DescriptorDimension dim = createInvalidCycleDimension();

		// cycle detected
		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(
				DescriptorDimensionException.class, 1014));

		final DescriptorGraph graph = new DescriptorGraph();
		graph.create(dim);
	}

	/**
	 * Tests the calculation of different min- and max-distances between nodes
	 * and levels.
	 */
	@Test
	public void testMinMaxDistance() {
		DescriptorGraphNode descriptorGraphNode;
		DescriptorGraphLevel descriptorGraphLevel;

		final DescriptorDimension dim = createMinMaxHierarchy();

		final DescriptorGraph graph = new DescriptorGraph();
		graph.create(dim);

		/*
		 * Check the nodes.
		 */
		descriptorGraphNode = graph.getNode("HIERARCHY_STRUCTURE",
				DescriptorDimension.ROOT_LEVEL_ID);
		assertNotNull(descriptorGraphNode);
		assertFalse(descriptorGraphNode.isSource());
		assertTrue(descriptorGraphNode.isSink());
		assertEquals(0, descriptorGraphNode.getMinDistance());
		assertEquals(0, descriptorGraphNode.getMaxDistance());

		descriptorGraphNode = graph.getNode("HIERARCHY_STRUCTURE", "MEM_A_1");
		assertNotNull(descriptorGraphNode);
		assertTrue(descriptorGraphNode.getChildren().toString(),
				descriptorGraphNode.isSource());
		assertFalse(descriptorGraphNode.getParents().toString(),
				descriptorGraphNode.isSink());
		assertEquals(2, descriptorGraphNode.getMinDistance());
		assertEquals(3, descriptorGraphNode.getMaxDistance());

		descriptorGraphNode = graph.getNode("HIERARCHY_STRUCTURE", "MEM_B2_1");
		assertNotNull(descriptorGraphNode);
		assertFalse(descriptorGraphNode.getChildren().toString(),
				descriptorGraphNode.isSource());
		assertFalse(descriptorGraphNode.getParents().toString(),
				descriptorGraphNode.isSink());
		assertEquals(1, descriptorGraphNode.getMinDistance());
		assertEquals(1, descriptorGraphNode.getMaxDistance());

		descriptorGraphNode = graph.getNode("HIERARCHY_STRUCTURE", "MEM_B2_2");
		assertNotNull(descriptorGraphNode);
		assertFalse(descriptorGraphNode.getChildren().toString(),
				descriptorGraphNode.isSource());
		assertFalse(descriptorGraphNode.getParents().toString(),
				descriptorGraphNode.isSink());
		assertEquals(1, descriptorGraphNode.getMinDistance());
		assertEquals(2, descriptorGraphNode.getMaxDistance());

		descriptorGraphNode = graph.getNode("HIERARCHY_STRUCTURE", "*");
		assertNotNull(descriptorGraphNode);
		assertFalse(descriptorGraphNode.getChildren().toString(),
				descriptorGraphNode.isSource());
		assertTrue(descriptorGraphNode.getParents().toString(),
				descriptorGraphNode.isSink());
		assertEquals(0, descriptorGraphNode.getMinDistance());
		assertEquals(0, descriptorGraphNode.getMaxDistance());

		/*
		 * Check the levels.
		 */
		descriptorGraphLevel = graph.getLevel("HIERARCHY_STRUCTURE", "*");
		assertNotNull(descriptorGraphLevel);
		assertEquals(1, descriptorGraphLevel.getDescriptorLevels().size());
		assertEquals("*", descriptorGraphLevel.getDescriptorLevelId());
		assertEquals(1, descriptorGraphLevel.getNodes().size());
		assertTrue(descriptorGraphLevel.isShared());
		assertTrue(descriptorGraphLevel.isRoot());
		assertFalse(descriptorGraphLevel.isEmpty());
		assertFalse(descriptorGraphLevel.isLeaf());
		assertEquals(0, descriptorGraphLevel.getMinDistance());
		assertEquals(0, descriptorGraphLevel.getMaxDistance());

		descriptorGraphLevel = graph.getLevel("HIERARCHY_STRUCTURE", "B2");
		assertNotNull(descriptorGraphLevel);
		assertEquals(1, descriptorGraphLevel.getDescriptorLevels().size());
		assertEquals("B2", descriptorGraphLevel.getDescriptorLevelId());
		assertEquals(2, descriptorGraphLevel.getNodes().size());
		assertFalse(descriptorGraphLevel.isShared());
		assertFalse(descriptorGraphLevel.isRoot());
		assertFalse(descriptorGraphLevel.isEmpty());
		assertFalse(descriptorGraphLevel.isLeaf());
		assertEquals(2, descriptorGraphLevel.getMaxDistance());
		assertEquals(1, descriptorGraphLevel.getMinDistance());
	}

	/**
	 * Tests the exception thrown if an invalid level hierarchy is defined (i.e.
	 * no DAG is defined for the hierarchy).
	 */
	@Test
	public void testInvalidLevelHierarchyNoDAG() {
		final DescriptorDimension dim = createInvalidLevelHierarchyNoDAG();

		// cycle detected
		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(
				DescriptorDimensionException.class, 1018));

		final DescriptorGraph graph = new DescriptorGraph();
		graph.create(dim);
	}

	/**
	 * Tests the exception to be thrown if there are no paths between each level
	 * within the level hierarchy.
	 */
	@Test
	public void testInvalidLevelHierarchyNoPath() {
		final DescriptorDimension dim = createInvalidLevelHierarchyNoPath();

		// no path between levels
		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(
				DescriptorDimensionException.class, 1020));

		final DescriptorGraph graph = new DescriptorGraph();
		graph.create(dim);
	}

	/**
	 * Tests a dimension with multiple hierarchies and a shared dimension.
	 */
	@Test
	public void testMulitpleHierarchy() {
		DescriptorHierarchy hierarchy;

		final DescriptorDimension dim = new DescriptorDimension(
				metaModel.getDescriptorModel("LOCATION"));

		// add one COUNTRY -> *
		hierarchy = dim.addHierarchy("H_COUNTRY_ONLY", null);
		hierarchy.addPatternMember("GERMANY", ".*, Deutschland", false,
				"COUNTRY", Arrays.asList("*"));
		hierarchy.addPatternMember("USA", ".*, USA", false, "COUNTRY",
				Arrays.asList("*"));

		// add one COUNTRY -> CONTINENT -> *
		hierarchy = dim.addHierarchy("H_CONTINENT", null);
		hierarchy.addMember("EUROPE", "CONTINENT", Arrays.asList("*"));
		hierarchy.addMember("AMERICA", "CONTINENT", Arrays.asList("*"));

		hierarchy.addPatternMember("GERMANY", ".*, Deutschland", false,
				"COUNTRY", Arrays.asList("EUROPE"));
		hierarchy.addPatternMember("USA", ".*, USA", false, "COUNTRY",
				Arrays.asList("AMERICA"));

		// mark COUNTRY as shared
		dim.addSharedLevel("COUNTRY");

		// create the graph
		final DescriptorGraph graph = new DescriptorGraph();
		graph.create(dim);

		// validate
		assertEquals(4, graph.getLevel("H_CONTINENT", "COUNTRY").getNodes()
				.size());
		assertEquals(2, graph.getLevel("H_CONTINENT", "COUNTRY")
				.getDescriptorLevels().size());
		assertEquals(1, graph.getLevel("H_CONTINENT", "COUNTRY")
				.getLeafMembers("H_CONTINENT", "GERMANY").size());
	}
}
