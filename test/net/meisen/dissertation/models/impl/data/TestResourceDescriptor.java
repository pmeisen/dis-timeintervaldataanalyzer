package net.meisen.dissertation.models.impl.data;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import net.meisen.dissertation.data.impl.descriptors.GeneralDescriptor;
import net.meisen.dissertation.data.impl.descriptors.ResourceDescriptor;
import net.meisen.dissertation.data.impl.idfactories.IntegerIdsFactory;

import org.junit.Test;

/**
 * Tests the implementation of a {@code Resource}
 * 
 * @author pmeisen
 * 
 */
public class TestResourceDescriptor {

	/**
	 * Tests the equality
	 */
	@Test
	public void testEquality() {
		final DescriptorModel<Integer> model = new DescriptorModel<Integer>(
				"RESMODEL_ID", GeneralDescriptor.class, new IntegerIdsFactory());

		// create some samples to be tested
		final ResourceDescriptor<Integer> res1 = new ResourceDescriptor<Integer>(
				model, 5, "My Value");
		final ResourceDescriptor<Integer> res2 = new ResourceDescriptor<Integer>(
				model, 6, "Another Value");
		final ResourceDescriptor<Integer> res3 = new ResourceDescriptor<Integer>(
				model, 5, "My Value");
		final ResourceDescriptor<Integer> res4 = new ResourceDescriptor<Integer>(
				new DescriptorModel<Integer>("ANOTHER_RESMODEL_ID",
						GeneralDescriptor.class, new IntegerIdsFactory()), 5,
				"My Value");

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
