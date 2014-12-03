package net.meisen.dissertation.model.dimensions;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.UUID;

import net.meisen.dissertation.exceptions.DescriptorDimensionException;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

import org.junit.Test;

/**
 * Tests the implementation of {@code DescriptorDimension}.
 * 
 * @author pmeisen
 * 
 */
public class TestDescriptorDimension extends BaseDescriptorDimensionTest {

	/**
	 * Tests the creation of a dimension.
	 */
	@Test
	public void testDimensionCreation() {
		DescriptorDimension dim;

		// create some dimensions
		dim = new DescriptorDimension(metaModel.getDescriptorModel("PERSON"));
		assertEquals("PERSON", dim.getId());
		assertEquals("Persons", dim.getName());
		dim = new DescriptorDimension(metaModel.getDescriptorModel("LOCATION"),
				"Locations");
		assertEquals("LOCATION", dim.getId());
		assertEquals("Locations", dim.getName());
		dim = new DescriptorDimension(metaModel.getDescriptorModel("SCREAMS"));
		assertEquals("SCREAMS", dim.getId());
		assertEquals("SCREAMS", dim.getName());
	}

	/**
	 * Tests the exception to be thrown if a {@code null} model is used.
	 */
	public void testDimensionCreationWithNullModel() {

		// create an invalid dimension
		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(
				DescriptorDimensionException.class, 1006));
		new DescriptorDimension(null);
	}

	/**
	 * Tests the adding of a hierarchy to the dimension.
	 */
	@Test
	public void testAddHierarchy() {
		DescriptorDimension dim;
		dim = new DescriptorDimension(metaModel.getDescriptorModel("PERSON"));
		assertNotNull(dim.addHierarchy("FAMILY", "Familie"));
		assertEquals(1, dim.getHierarchies().size());

		dim = new DescriptorDimension(metaModel.getDescriptorModel("PERSON"));
		assertNotNull(dim.addHierarchy("FAMILY", "Familie"));
		assertNotNull(dim.addHierarchy("FRIENDS", "Friends"));
		assertEquals(2, dim.getHierarchies().size());
	}

	/**
	 * Tests the exception to be thrown if a hierarchy with the same name is
	 * added.
	 */
	@Test
	public void testExceptionWhenAddDuplicateHierarchy() {
		DescriptorDimension dim;
		dim = new DescriptorDimension(metaModel.getDescriptorModel("PERSON"));
		dim.addHierarchy("FAMILY", "Familie");

		// add the same hierarchy again
		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(
				DescriptorDimensionException.class, 1000));
		dim.addHierarchy("FAMILY", "Family");
	}

	/**
	 * Tests the adding of a shared level to the dimension.
	 */
	@Test
	public void testAddSharedLevel() {
		DescriptorDimension dim;
		dim = new DescriptorDimension(metaModel.getDescriptorModel("PERSON"));

		final String sharedLevelId = UUID.randomUUID().toString();
		dim.addSharedLevel(sharedLevelId);
		for (int i = 0; i < 100; i++) {
			assertFalse(dim.isSharedLevel(UUID.randomUUID().toString()));
		}
		assertTrue(dim.isSharedLevel(sharedLevelId));
	}

	/**
	 * Tests the exception to be thrown if a shared level is added twice.
	 */
	@Test
	public void testExceptionWhenAddDuplicateSharedLevel() {
		DescriptorDimension dim;
		dim = new DescriptorDimension(metaModel.getDescriptorModel("PERSON"));

		final String sharedLevelId = UUID.randomUUID().toString();
		dim.addSharedLevel(sharedLevelId);

		// add the same shared level again
		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(
				DescriptorDimensionException.class, 1001));
		dim.addSharedLevel(sharedLevelId);
	}

	/**
	 * Tests the member and level creation.
	 */
	@Test
	public void testMemberAndLevelCreation() {
		final DescriptorDimension dim = createLocationDimension();

		final DescriptorHierarchy hierarchy = dim
				.getHierarchy("HIERARCHY_LOCATION");
		assertNotNull(hierarchy);

		// check the levels
		final Collection<DescriptorLevel> levels = hierarchy.getLevels();
		assertEquals(levels.toString(), 4, levels.size());

		// check the members
		final Collection<DescriptorMember> members = hierarchy.getMembers();
		assertEquals(members.toString(), 9, members.size());
	}
}
