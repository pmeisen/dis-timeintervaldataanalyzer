package net.meisen.dissertation.data.impl.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.models.impl.data.Resource;
import net.meisen.dissertation.models.impl.data.ResourceModel;
import net.meisen.general.sbconfigurator.api.IConfiguration;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the implementation of the {@code ResourcesFactory}.
 * 
 * @author pmeisen
 * 
 */
@RunWith(JUnitConfigurationRunner.class)
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
@SystemProperty(property = "testBeans.selector", value = "net/meisen/dissertation/data/impl/resources/testResourcesFactories.xml")
public class TestResoucesFactory {

	@Autowired(required = true)
	@Qualifier("coreConfiguration")
	private IConfiguration configuration;

	/**
	 * Tests a {@code ResourcesFactory} using a {@code UuIdsFactory}.
	 */
	@Test
	public void testResourcesFactoryWithUuIds() {
		final ResourcesFactory factory = configuration
				.getModule("resourcesFactoryWithUuIds");

		// create a model and a resource with the factory
		final ResourceModel model = new ResourceModel("someId", "some name");

		@SuppressWarnings("unchecked")
		final Resource<UUID> resource = (Resource<UUID>) factory
				.createResource(model, "A sample value");

		// check the settings of the resource
		assertEquals("some name", resource.getModelName());
		assertEquals("A sample value", resource.getValue());

		// now check some ids
		final Set<UUID> ids = new HashSet<UUID>();
		for (int i = 0; i < 1000; i++) {
			@SuppressWarnings("unchecked")
			final Resource<UUID> r = (Resource<UUID>) factory.createResource(
					model, UUID.randomUUID().toString());
			assertTrue(ids.add(r.getId()));
		}
	}
}
