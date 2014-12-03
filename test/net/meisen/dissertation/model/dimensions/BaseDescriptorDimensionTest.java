package net.meisen.dissertation.model.dimensions;

import static org.junit.Assert.assertNotNull;

import java.util.Arrays;

import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.dimensions.DescriptorDimension;
import net.meisen.dissertation.model.dimensions.DescriptorHierarchy;

import org.junit.Before;

/**
 * Base implementation for tests considering dimension.
 * 
 * @author pmeisen
 * 
 */
public class BaseDescriptorDimensionTest extends LoaderBasedTest {

	/**
	 * The loaded {@code MetaDataModel} used for testing.
	 */
	protected MetaDataModel metaModel;

	/**
	 * Loads the {@code MetaDataModel} for the test.
	 */
	@Before
	public void getDescriptors() {
		final TidaModel model = m("/net/meisen/dissertation/model/dimensions/testDescriptorModel.xml");
		metaModel = model.getMetaDataModel();

		assertNotNull(metaModel);
	}

	/**
	 * Creates a dimension as illustrated by <br/>
	 * <img src="images/testLocationDimension.png" style="width: 600px" /><br/>
	 * 
	 * @return a dimension - described as above - based on the location
	 *         {@code DescriptorModel}
	 */
	public DescriptorDimension createLocationDimension() {
		final DescriptorDimension dim = new DescriptorDimension(
				metaModel.getDescriptorModel("LOCATION"));
		final DescriptorHierarchy hierarchy = dim.addHierarchy(
				"HIERARCHY_LOCATION", "Location");

		// rename the root-level and the unassigned level
		hierarchy.modifyLevel(DescriptorDimension.ROOT_LEVEL_ID, "World");
		hierarchy.modifyLevel(DescriptorDimension.UNASSIGNED_LEVEL_ID,
				"Unassigned");

		// add a descriptor
		hierarchy.addDescriptorMember("LAND_VAT", "Vatikanstaat", "Land", null);

		// add a pattern
		hierarchy.addPatternMember("STADT_DE", "\\1", "(.*), Deutschland",
				false, "Stadt", Arrays.asList("LAND_DE"));
		hierarchy.addPatternMember("STADT_USA", "\\1", "(.*), USA", false,
				"Stadt", Arrays.asList("USA"));
		hierarchy.addPatternMember("STADT_ANDERE", ".*", false, "Stadt",
				Arrays.asList("Unbekanntes"));

		// add some members
		hierarchy.addMember("Unbekanntes", "Land", null);
		hierarchy.addMember("LAND_FR", "Frankreich", "Land", null);
		hierarchy.addMember("LAND_DE", "Deutschland", "Land", null);
		hierarchy.addMember("USA", "Land", Arrays.asList("*"));

		return dim;
	}

	/**
	 * Creates a dimension as illustrated by <br/>
	 * <img src="images/testBookDimension.png" style="width: 600px" /><br/>
	 * 
	 * @return a dimension - described as above - based on the book
	 *         {@code DescriptorModel}
	 */
	public DescriptorDimension createBookDimension() {
		DescriptorHierarchy hierarchy;

		final DescriptorDimension dim = new DescriptorDimension(
				metaModel.getDescriptorModel("BOOK"));
		hierarchy = dim.addHierarchy("HIERARCHY_STRUCTURE", "Book-Structure");

		// add the meta members
		hierarchy.addDescriptorMember("TITLE", "Title", "TITLE", "Meta", null);
		hierarchy.addDescriptorMember("TOC", "Table Of Content", "TOC", "Meta",
				Arrays.asList("0", "1"));

		// add the chapter members
		hierarchy.addPatternMember("A0-9", "A[0-9]", false, "Paragraphs",
				Arrays.asList("1"));
		hierarchy.addPatternMember("B0-9", "B[0-9]", false, "Paragraphs",
				Arrays.asList("1"));

		// add the preamble and the content level
		hierarchy.addMember("0", "Preamble", "Preamble", null);
		hierarchy.addMember("1", "Chapter 1", "Content", null);
		hierarchy.addDescriptorMember("2", "Chapter 2", "2", "Content", null);

		return dim;
	}

	/**
	 * Creates an invalid dimension, which has multiple root members, as
	 * illustrated by <br/>
	 * <img src="images/testMultipleRootDimension.png" style="width: 300px" /><br/>
	 * 
	 * @return a dimension - described as above - based on the book
	 *         {@code DescriptorModel}
	 */
	public DescriptorDimension createInvalidMultipleRootDimension() {
		DescriptorHierarchy hierarchy;

		final DescriptorDimension dim = new DescriptorDimension(
				metaModel.getDescriptorModel("BOOK"));
		hierarchy = dim.addHierarchy("HIERARCHY_STRUCTURE", "Book-Structure");

		// add another root
		hierarchy.addDescriptorMember("TITLE", "Title",
				DescriptorDimension.ROOT_LEVEL_ID, null);

		return dim;
	}

	/**
	 * Creates an invalid dimension which contains no hierarchy at all.
	 * 
	 * @return a dimension - described as above - based on the book
	 *         {@code DescriptorModel}
	 */
	public DescriptorDimension createInvalidEmptyDimension() {
		return new DescriptorDimension(metaModel.getDescriptorModel("BOOK"));
	}

	/**
	 * Creates an invalid dimension as illustrated by <br/>
	 * <img src="images/testOnlyRootDimension.png" style="width: 200px" /><br/>
	 * 
	 * @return a dimension - described as above - based on the book
	 *         {@code DescriptorModel}
	 */
	public DescriptorDimension createInvalidOnlyRootDimension() {
		final DescriptorDimension dim = new DescriptorDimension(
				metaModel.getDescriptorModel("BOOK"));
		dim.addHierarchy("HIERARCHY_STRUCTURE", "Book-Structure");

		return dim;
	}

	/**
	 * Creates an invalid dimension having a defined cycle as illustrated by <br/>
	 * <img src="images/testCycleDimension.png" style="width: 350px" /><br/>
	 * 
	 * @return a dimension - described as above - based on the book
	 *         {@code DescriptorModel}
	 */
	public DescriptorDimension createInvalidCycleDimension() {
		DescriptorHierarchy hierarchy;

		final DescriptorDimension dim = new DescriptorDimension(
				metaModel.getDescriptorModel("BOOK"));
		hierarchy = dim.addHierarchy("HIERARCHY_STRUCTURE", "Book-Structure");

		// levels
		hierarchy.addMember("CONTENT", "Content", "Content",
				Arrays.asList("PAGE", "*"));
		hierarchy.addMember("PAGE", "Page", "Page", Arrays.asList("CONTENT"));

		// add the members
		hierarchy.addDescriptorMember("TITLE", "Title", "Data",
				Arrays.asList("PAGE"));
		hierarchy.addPatternMember("REST", ".*", false, "Data",
				Arrays.asList("PAGE"));

		return dim;
	}

	/**
	 * Creates a dimension having different min-max values for a node and a
	 * level as illustrated by <br/>
	 * <br/>
	 * <img src="images/testMinMaxHierarchy.png" style="width: 300px" /><br/>
	 * 
	 * @return a dimension - described as above - based on the book
	 *         {@code DescriptorModel}
	 */
	public DescriptorDimension createMinMaxHierarchy() {
		DescriptorHierarchy hierarchy;

		final DescriptorDimension dim = new DescriptorDimension(
				metaModel.getDescriptorModel("BOOK"));
		hierarchy = dim.addHierarchy("HIERARCHY_STRUCTURE", "Misc-Structure");

		hierarchy.addMember("MEM_B1_1", "B1", null);
		hierarchy.addMember("MEM_B2_1", "B2", null);
		hierarchy.addMember("MEM_B2_2", "B2", Arrays.asList("*", "MEM_B1_1"));
		hierarchy.addPatternMember("MEM_A_1", ".*", false, "A",
				Arrays.asList("MEM_B2_1", "MEM_B2_2"));

		return dim;
	}

	/**
	 * Creates an invalid dimension having an invalid level hierarchy which does
	 * not define a DAG, see<br/>
	 * <br/>
	 * <img src="images/testInvalidLevelHierarchyNoDAG.png" style="width: 200px"
	 * /><br/>
	 * 
	 * @return a dimension - described as above - based on the book
	 *         {@code DescriptorModel}
	 */
	public DescriptorDimension createInvalidLevelHierarchyNoDAG() {
		DescriptorHierarchy hierarchy;

		final DescriptorDimension dim = new DescriptorDimension(
				metaModel.getDescriptorModel("BOOK"));
		hierarchy = dim.addHierarchy("HIERARCHY_STRUCTURE", "Misc-Structure");

		hierarchy.addMember("MEM_B_2", "B", null);

		hierarchy.addMember("MEM_A_2", "A", Arrays.asList("*", "MEM_B_2"));

		hierarchy.addMember("MEM_B_1", "B", Arrays.asList("*", "MEM_A_2"));

		hierarchy.addPatternMember("MEM_A_1", ".*", false, "A",
				Arrays.asList("MEM_B_1"));

		return dim;
	}

	/**
	 * Creates an invalid dimension having an invalid level hierarchy as
	 * illustrated and explained by <br/>
	 * <br/>
	 * <img src="images/testInvalidLevelHierarchyPath.png" style="width: 600px"
	 * /><br/>
	 * 
	 * @return a dimension - described as above - based on the book
	 *         {@code DescriptorModel}
	 */
	public DescriptorDimension createInvalidLevelHierarchyNoPath() {
		DescriptorHierarchy hierarchy;

		final DescriptorDimension dim = new DescriptorDimension(
				metaModel.getDescriptorModel("BOOK"));
		hierarchy = dim.addHierarchy("HIERARCHY_STRUCTURE", "Misc-Structure");

		hierarchy.addMember("MEM_B1_1", "B1", null);
		hierarchy.addMember("MEM_B2_1", "B2", null);
		hierarchy.addPatternMember("MEM_B2_2", "B.*", false, "B2",
				Arrays.asList("*", "MEM_B1_1"));
		hierarchy.addPatternMember("MEM_A_1", ".*", false, "A",
				Arrays.asList("MEM_B2_1"));

		return dim;
	}
}
