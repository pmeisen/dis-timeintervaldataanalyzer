package net.meisen.dissertation.performance;

import java.util.UUID;

import net.meisen.dissertation.impl.descriptors.GeneralDescriptor;
import net.meisen.dissertation.impl.idfactories.UuIdsFactory;
import net.meisen.dissertation.impl.indexes.IndexFactory;
import net.meisen.dissertation.model.descriptors.DescriptorModel;

import org.junit.Test;

public class TestLargeDescriptor {

	@Test
	public void testLargeDescriptor() {
		final long max = Integer.MAX_VALUE;
		final DescriptorModel<UUID> model = new DescriptorModel<UUID>("ID",
				"MODEL", GeneralDescriptor.class, new UuIdsFactory(),
				new IndexFactory());

		for (long i = 0; i < max; i++) {
			model.createDescriptor(UUID.randomUUID());
			
			if ((i + 1) % 1000000l == 0l) {
				System.out.println("Created " + (i + 1) + " of " + max + "' descriptors...");
			}
		}
	}
}
