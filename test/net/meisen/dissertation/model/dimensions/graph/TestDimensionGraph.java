package net.meisen.dissertation.model.dimensions.graph;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import net.meisen.dissertation.exceptions.DescriptorDimensionException;
import net.meisen.dissertation.model.dimensions.BaseDimensionTest;
import net.meisen.dissertation.model.dimensions.DescriptorDimension;
import net.meisen.dissertation.model.dimensions.graph.DimensionGraph;
import net.meisen.dissertation.model.dimensions.graph.Level;
import net.meisen.dissertation.model.dimensions.graph.Node;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

import org.junit.Test;

/**
 * Tests the generated {@code DimensionGraph}.
 * 
 * @author pmeisen
 * 
 */
public class TestDimensionGraph extends BaseDimensionTest {

	/**
	 * Tests the nodes and levels of a location-based dimension.
	 */
	@Test
	public void testNodesAndLevelsOfLocation() {
		Node node;
		Level level;

		final DescriptorDimension dim = createLocationDimension();

		final DimensionGraph graph = new DimensionGraph();
		graph.create(dim);

		assertEquals(9, graph.getNodes().size());
		assertEquals(3, graph.getLevels().size());

		/*
		 * Check the nodes.
		 */
		node = graph.getNode("HIERARCHY_LOCATION",
				DescriptorDimension.ROOT_LEVEL_ID);
		assertNotNull(node);
		assertFalse(node.isSource());
		assertTrue(node.isSink());
		assertEquals(0, node.getMinDistance());
		assertEquals(0, node.getMaxDistance());

		node = graph.getNode("HIERARCHY_LOCATION", "STADT_DE");
		assertNotNull(node);
		assertTrue(node.isSource());
		assertFalse(node.isSink());
		assertEquals(2, node.getMinDistance());
		assertEquals(2, node.getMaxDistance());

		node = graph.getNode("HIERARCHY_LOCATION", "LAND_VAT");
		assertNotNull(node);
		assertTrue(node.getChildren().toString(), node.isSource());
		assertFalse(node.getParents().toString(), node.isSink());
		assertEquals(1, node.getMinDistance());
		assertEquals(1, node.getMaxDistance());

		node = graph.getNode("HIERARCHY_LOCATION", "LAND_FR");
		assertNotNull(node);
		assertTrue(node.getChildren().toString(), node.isSource());
		assertFalse(node.getParents().toString(), node.isSink());
		assertEquals(1, node.getMinDistance());
		assertEquals(1, node.getMaxDistance());

		node = graph.getNode("HIERARCHY_LOCATION", "LAND_DE");
		assertNotNull(node);
		assertFalse(node.getChildren().toString(), node.isSource());
		assertFalse(node.getParents().toString(), node.isSink());
		assertEquals(1, node.getMinDistance());
		assertEquals(1, node.getMaxDistance());

		node = graph.getNode("HIERARCHY_LOCATION", "USA");
		assertNotNull(node);
		assertFalse(node.getChildren().toString(), node.isSource());
		assertFalse(node.getParents().toString(), node.isSink());
		assertEquals(1, node.getMinDistance());
		assertEquals(1, node.getMaxDistance());

		/*
		 * Check the levels.
		 */
		level = graph.getLevel("HIERARCHY_LOCATION",
				DescriptorDimension.ROOT_LEVEL_ID);
		assertNotNull(level);
		assertEquals(1, level.getDescriptorLevels().size());
		assertEquals("*", level.getDescriptorLevelId());
		assertEquals(1, level.getNodes().size());
		assertTrue(level.isShared());
		assertTrue(level.isRoot());
		assertFalse(level.isEmpty());
		assertFalse(level.isLeaf());
		assertEquals(0, level.getMaxDistance());
		assertEquals(0, level.getMinDistance());

		level = graph.getLevel("HIERARCHY_LOCATION", "Land");
		assertNotNull(level);
		assertEquals(1, level.getDescriptorLevels().size());
		assertEquals("Land", level.getDescriptorLevelId());
		assertEquals(5, level.getNodes().size());
		assertFalse(level.isShared());
		assertFalse(level.isRoot());
		assertFalse(level.isEmpty());
		assertFalse(level.isLeaf());
		assertEquals(1, level.getMaxDistance());
		assertEquals(1, level.getMinDistance());

		level = graph.getLevel("HIERARCHY_LOCATION", "Stadt");
		assertNotNull(level);
		assertEquals(1, level.getDescriptorLevels().size());
		assertEquals("Stadt", level.getDescriptorLevelId());
		assertEquals(3, level.getNodes().size());
		assertFalse(level.isShared());
		assertFalse(level.isRoot());
		assertFalse(level.isEmpty());
		assertTrue(level.isLeaf());
		assertEquals(2, level.getMaxDistance());
		assertEquals(2, level.getMinDistance());

		level = graph.getLevel("HIERARCHY_LOCATION",
				DescriptorDimension.UNASSIGNED_LEVEL_ID);
		assertNull(level);
	}

	/**
	 * Tests the nodes and levels of a book-based dimension.
	 */
	@Test
	public void testNodesAndLevelsOfBook() {
		Node node;
		Level level;

		final DescriptorDimension dim = createBookDimension();

		final DimensionGraph graph = new DimensionGraph();
		graph.create(dim);

		assertEquals(8, graph.getNodes().size());
		assertEquals(5, graph.getLevels().size());

		/*
		 * Check the nodes.
		 */
		node = graph.getNode("HIERARCHY_STRUCTURE",
				DescriptorDimension.ROOT_LEVEL_ID);
		assertNotNull(node);
		assertFalse(node.isSource());
		assertTrue(node.isSink());
		assertEquals(0, node.getMinDistance());
		assertEquals(0, node.getMaxDistance());

		node = graph.getNode("HIERARCHY_STRUCTURE", "0");
		assertNotNull(node);
		assertFalse(node.getChildren().toString(), node.isSource());
		assertFalse(node.getParents().toString(), node.isSink());
		assertEquals(1, node.getMinDistance());
		assertEquals(1, node.getMaxDistance());

		/*
		 * Check the levels.
		 */
		level = graph.getLevel("HIERARCHY_STRUCTURE", "Meta");
		assertNotNull(level);
		assertEquals(1, level.getDescriptorLevels().size());
		assertEquals("Meta", level.getDescriptorLevelId());
		assertEquals(2, level.getNodes().size());
		assertFalse(level.isShared());
		assertFalse(level.isRoot());
		assertFalse(level.isEmpty());
		assertTrue(level.isLeaf());
		assertEquals(2, level.getMaxDistance());
		assertEquals(1, level.getMinDistance());
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

		final DimensionGraph graph = new DimensionGraph();
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

		final DimensionGraph graph = new DimensionGraph();
		graph.create(dim);
	}

	/**
	 * Tests the calculation of different min- and max-distances between nodes
	 * and levels.
	 */
	@Test
	public void testMinMaxDistance() {
		Node node;
		Level level;

		final DescriptorDimension dim = createMinMaxHierarchy();

		final DimensionGraph graph = new DimensionGraph();
		graph.create(dim);

		/*
		 * Check the nodes.
		 */
		node = graph.getNode("HIERARCHY_STRUCTURE",
				DescriptorDimension.ROOT_LEVEL_ID);
		assertNotNull(node);
		assertFalse(node.isSource());
		assertTrue(node.isSink());
		assertEquals(0, node.getMinDistance());
		assertEquals(0, node.getMaxDistance());

		node = graph.getNode("HIERARCHY_STRUCTURE", "MEM_A_1");
		assertNotNull(node);
		assertTrue(node.getChildren().toString(), node.isSource());
		assertFalse(node.getParents().toString(), node.isSink());
		assertEquals(2, node.getMinDistance());
		assertEquals(3, node.getMaxDistance());

		node = graph.getNode("HIERARCHY_STRUCTURE", "MEM_B2_1");
		assertNotNull(node);
		assertFalse(node.getChildren().toString(), node.isSource());
		assertFalse(node.getParents().toString(), node.isSink());
		assertEquals(1, node.getMinDistance());
		assertEquals(1, node.getMaxDistance());

		node = graph.getNode("HIERARCHY_STRUCTURE", "MEM_B2_2");
		assertNotNull(node);
		assertFalse(node.getChildren().toString(), node.isSource());
		assertFalse(node.getParents().toString(), node.isSink());
		assertEquals(1, node.getMinDistance());
		assertEquals(2, node.getMaxDistance());

		node = graph.getNode("HIERARCHY_STRUCTURE", "*");
		assertNotNull(node);
		assertFalse(node.getChildren().toString(), node.isSource());
		assertTrue(node.getParents().toString(), node.isSink());
		assertEquals(0, node.getMinDistance());
		assertEquals(0, node.getMaxDistance());

		/*
		 * Check the levels.
		 */
		level = graph.getLevel("HIERARCHY_STRUCTURE", "*");
		assertNotNull(level);
		assertEquals(1, level.getDescriptorLevels().size());
		assertEquals("*", level.getDescriptorLevelId());
		assertEquals(1, level.getNodes().size());
		assertTrue(level.isShared());
		assertTrue(level.isRoot());
		assertFalse(level.isEmpty());
		assertFalse(level.isLeaf());
		assertEquals(0, level.getMinDistance());
		assertEquals(0, level.getMaxDistance());

		level = graph.getLevel("HIERARCHY_STRUCTURE", "B2");
		assertNotNull(level);
		assertEquals(1, level.getDescriptorLevels().size());
		assertEquals("B2", level.getDescriptorLevelId());
		assertEquals(2, level.getNodes().size());
		assertFalse(level.isShared());
		assertFalse(level.isRoot());
		assertFalse(level.isEmpty());
		assertFalse(level.isLeaf());
		assertEquals(2, level.getMaxDistance());
		assertEquals(1, level.getMinDistance());
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

		final DimensionGraph graph = new DimensionGraph();
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

		final DimensionGraph graph = new DimensionGraph();
		graph.create(dim);
	}
}
