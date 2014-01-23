package net.meisen.dissertation.impl.idfactories;

import static org.junit.Assert.assertTrue;

import java.util.HashSet;
import java.util.Locale;
import java.util.Random;
import java.util.Set;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.exceptions.IdsFactoryException;
import net.meisen.dissertation.impl.idfactories.IntegerIdsFactory;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.matchers.JUnitMatchers;
import org.junit.rules.ExpectedException;
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
@SystemProperty(property = "testBeans.selector", value = "net/meisen/dissertation/data/impl/idfactories/testIdFactories.xml")
public class TestIntegerIdsFactory {

	@Autowired(required = true)
	@Qualifier("integerIdFactory")
	private IntegerIdsFactory factory;

	private Locale oldDefault;

	/**
	 * Rule to evaluate exceptions
	 */
	@Rule
	public ExpectedException thrown = ExpectedException.none();

	/**
	 * Make sure we have {@code Locale.US} so that comparisons of errors will
	 * fit
	 */
	@Before
	public void setUp() {
		oldDefault = Locale.getDefault();
		Locale.setDefault(Locale.US);
	}

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
	 * Reset the {@code Locale}
	 */
	@After
	public void cleanUp() {
		Locale.setDefault(oldDefault);
	}
}
