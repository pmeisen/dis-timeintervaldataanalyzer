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
 * Tests the implementation of the {@code IntegerIdsFactory}.
 * 
 * @author pmeisen
 * 
 */
@RunWith(JUnitConfigurationRunner.class)
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
@SystemProperty(property = "testBeans.selector", value = "net/meisen/dissertation/impl/idfactories/testIdFactories.xml")
public class TestIntegerIdsFactory extends ExceptionBasedTest {

	@Autowired(required = true)
	@Qualifier("integerIdFactory")
	private IntegerIdsFactory factory;

	/**
	 * Tests the unique id generation of the factory
	 */
	@Test
	public void testIdGeneration() {
		final Random rnd = new Random();

		// initialize the factory with some random seed
		final int seed = Math.min(Integer.MAX_VALUE - 10000,
				Math.max(IntegerIdsFactory.FIRST_ID, rnd.nextInt()));
		factory.initialize(seed);

		// generate some identifiers
		final Set<Integer> generatedIds = new HashSet<Integer>();
		for (int i = 0; i < 10000; i++) {
			final int id = factory.getId();

			assertTrue(id > seed);
			assertTrue(generatedIds.add(id));
		}
	}

	/**
	 * Tests the initialization with an invalid last identifier
	 */
	@Test
	public void testInvalidInitialization() {
		final int invalidId = IntegerIdsFactory.FIRST_ID - 1;

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
		factory.initialize(Integer.MAX_VALUE);

		thrown.expect(IdsFactoryException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("amount of available identifiers is reached"));

		factory.getId();
	}

	/**
	 * Tests the implementation of
	 * {@link IntegerIdsFactory#setIdAsUsed(Integer)}.
	 */
	@Test
	public void testSetUsedId() {
		factory.initialize(IntegerIdsFactory.FIRST_ID);
		assertEquals(new Integer(IntegerIdsFactory.FIRST_ID + 1),
				factory.getId());

		// check the usage of the next short
		factory.setIdAsUsed(5);
		assertEquals(new Integer(6), factory.getId());
		factory.setIdAsUsed(5);
		assertEquals(new Integer(7), factory.getId());
		factory.setIdAsUsed(7);
		assertEquals(new Integer(8), factory.getId());
		factory.setIdAsUsed(100);
		assertEquals(new Integer(101), factory.getId());
		factory.setIdAsUsed(Integer.MAX_VALUE - 1);
		assertEquals(new Integer(Integer.MAX_VALUE), factory.getId());

		// also the max should be setable
		factory.setIdAsUsed(Integer.MAX_VALUE);
	}
}
