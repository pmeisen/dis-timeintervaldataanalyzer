package net.meisen.dissertation.models.impl.data;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * Tests the implementation of a {@code Resource}
 * 
 * @author pmeisen
 * 
 */
public class TestResource {

	/**
	 * Tests the equality
	 */
	@Test
	public void testEquality() {
		final ResourceModel model = new ResourceModel("RESMODEL_ID");

		// create some samples to be tested
		final Resource<Integer> res1 = new Resource<Integer>(model, 5,
				"My Value");
		final Resource<Integer> res2 = new Resource<Integer>(model, 6,
				"Another Value");
		final Resource<Integer> res3 = new Resource<Integer>(model, 5,
				"My Value");
		final Resource<Integer> res4 = new Resource<Integer>(new ResourceModel(
				"ANOTHER_RESMODEL_ID"), 5, "My Value");

		// null
		assertFalse(res1.equals(null));

		// identity
		assertTrue(res1.equals(res1));
		assertTrue(res2.equals(res2));
		assertTrue(res3.equals(res3));

		// id equals
		assertTrue(res1.equals(res3));
		assertTrue(res3.equals(res1));
		assertFalse(res3.equals(res2));
		assertFalse(res2.equals(res3));
		assertFalse(res2.equals(res1));
		assertFalse(res1.equals(res2));

		// model different
		assertFalse(res1.equals(res4));
		assertFalse(res4.equals(res1));
	}
}
