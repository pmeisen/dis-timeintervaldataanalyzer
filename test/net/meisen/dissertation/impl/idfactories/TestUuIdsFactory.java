package net.meisen.dissertation.impl.idfactories;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.impl.idfactories.UuIdsFactory;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the implementation of the {@code UuIdsFactory}.
 * 
 * @author pmeisen
 * 
 */
@RunWith(JUnitConfigurationRunner.class)
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
@SystemProperty(property = "testBeans.selector", value = "net/meisen/dissertation/impl/idfactories/testIdFactories.xml")
public class TestUuIdsFactory {

	@Autowired(required = true)
	@Qualifier("uuIdFactory")
	private UuIdsFactory factory;

	/**
	 * Tests the unique id generation of the factory
	 */
	@Test
	public void testIdGeneration() {

		// generate some identifiers
		final Set<UUID> generatedIds = new HashSet<UUID>();
		for (int i = 0; i < 10000; i++) {
			final UUID id = factory.getId();

			assertNotNull(id);
			assertTrue(generatedIds.add(id));
		}
	}
}
