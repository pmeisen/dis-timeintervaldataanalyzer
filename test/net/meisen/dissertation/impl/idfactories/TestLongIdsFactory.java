package net.meisen.dissertation.impl.idfactories;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.HashSet;
import java.util.Random;
import java.util.Set;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.exceptions.IdsFactoryException;
import net.meisen.dissertation.help.ExceptionBasedTest;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.Test;
import org.junit.matchers.JUnitMatchers;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the implementation of the {@code LongIdsFactory}.
 * 
 * @author pmeisen
 * 
 */
@RunWith(JUnitConfigurationRunner.class)
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
@SystemProperty(property = "testBeans.selector", value = "net/meisen/dissertation/impl/idfactories/testIdFactories.xml")
public class TestLongIdsFactory extends ExceptionBasedTest {

	@Autowired(required = true)
	@Qualifier("longIdFactory")
	private LongIdsFactory factory;

	/**
	 * Tests the unique id generation of the factory
	 */
	@Test
	public void testIdGeneration() {
		final Random rnd = new Random();

		// initialize the factory with some random seed
		final Long seed = Math.min(Long.MAX_VALUE - 10000,
				Math.max(LongIdsFactory.FIRST_ID, rnd.nextLong()));
		factory.initialize(seed);

		// generate some identifiers
		final Set<Long> generatedIds = new HashSet<Long>();
		for (int i = 0; i < 10000; i++) {
			final long id = factory.getId();

			assertTrue(id > seed);
			assertTrue(generatedIds.add(id));
		}
	}

	/**
	 * Tests the initialization with an invalid last identifier
	 */
	@Test
	public void testInvalidInitialization() {
		final long invalidId = LongIdsFactory.FIRST_ID - 1;

		thrown.expect(IdsFactoryException.class);
		thrown.expectMessage(JUnitMatchers.containsString("The identifier '"
				+ invalidId + "' is invalid"));

		factory.initialize(invalidId);
	}

	/**
	 * Tests the overflow of the identifiers, i.e. if no new identifiers are
	 * available
	 */
	@Test
	public void testOverflow() {
		factory.initialize(Long.MAX_VALUE);

		thrown.expect(IdsFactoryException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("amount of available identifiers is reached"));

		factory.getId();
	}

	/**
	 * Tests the implementation of {@link LongIdsFactory#setIdAsUsed(Long)}.
	 */
	@Test
	public void testSetUsedId() {
		factory.initialize(LongIdsFactory.FIRST_ID);
		assertEquals(new Long(LongIdsFactory.FIRST_ID + 1), factory.getId());

		// check the usage of the next short
		factory.setIdAsUsed(5l);
		assertEquals(new Long(6l), factory.getId());
		factory.setIdAsUsed(5l);
		assertEquals(new Long(7l), factory.getId());
		factory.setIdAsUsed(7l);
		assertEquals(new Long(8l), factory.getId());
		factory.setIdAsUsed(100l);
		assertEquals(new Long(101l), factory.getId());
		factory.setIdAsUsed(Long.MAX_VALUE - 1);
		assertEquals(new Long(Long.MAX_VALUE), factory.getId());

		// also the max should be setable
		factory.setIdAsUsed(Long.MAX_VALUE);
	}
}
