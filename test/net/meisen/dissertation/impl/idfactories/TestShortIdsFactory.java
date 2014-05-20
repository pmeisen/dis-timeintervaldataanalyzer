package net.meisen.dissertation.impl.idfactories;

import static org.junit.Assert.assertEquals;
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
public class TestShortIdsFactory extends ExceptionBasedTest {

	@Autowired(required = true)
	@Qualifier("shortIdFactory")
	private ShortIdsFactory factory;

	/**
	 * Tests the initialization with an invalid last identifier
	 */
	@Test
	public void testInvalidInitialization() {
		final short invalidId = ShortIdsFactory.FIRST_ID - 1;

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
		factory.initialize(Short.MAX_VALUE);

		thrown.expect(IdsFactoryException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("amount of available identifiers is reached"));

		factory.getId();
	}

	/**
	 * Tests the implementation of {@link ShortIdsFactory#setIdAsUsed(Short)}.
	 */
	@Test
	public void testSetUsedId() {
		factory.initialize(ShortIdsFactory.FIRST_ID);
		assertEquals(new Short((short) (ShortIdsFactory.FIRST_ID + 1)),
				factory.getId());

		// check the usage of the next short
		factory.setIdAsUsed((short) 5);
		assertEquals(new Short((short) 6), factory.getId());
		factory.setIdAsUsed((short) 5);
		assertEquals(new Short((short) 7), factory.getId());
		factory.setIdAsUsed((short) 7);
		assertEquals(new Short((short) 8), factory.getId());
		factory.setIdAsUsed((short) 100);
		assertEquals(new Short((short) 101), factory.getId());
		factory.setIdAsUsed((short) (Short.MAX_VALUE - 1));
		assertEquals(new Short(Short.MAX_VALUE), factory.getId());
		
		// also the max should be setable		
		factory.setIdAsUsed((short) (Short.MAX_VALUE));
	}
}
