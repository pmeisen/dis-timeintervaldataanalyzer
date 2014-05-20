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
public class TestByteIdsFactory extends ExceptionBasedTest {

	@Autowired(required = true)
	@Qualifier("byteIdFactory")
	private ByteIdsFactory factory;

	/**
	 * Tests the unique id generation of the factory
	 */
	@Test
	public void testIdGeneration() {
		final Random rnd = new Random();

		// initialize the factory with some random seed
		final byte[] rndBytes = new byte[1];
		rnd.nextBytes(rndBytes);
		final byte seed = (byte) Math.min(Byte.MAX_VALUE - 100,
				Math.max(ByteIdsFactory.FIRST_ID, rndBytes[0]));
		factory.initialize(seed);

		// generate some identifiers
		final Set<Byte> generatedIds = new HashSet<Byte>();
		for (byte i = 0; i < 100; i++) {
			final byte id = factory.getId();

			assertTrue(id > seed);
			assertTrue(generatedIds.add(id));
		}
	}

	/**
	 * Tests the initialization with an invalid last identifier
	 */
	@Test
	public void testInvalidInitialization() {
		final byte invalidId = ByteIdsFactory.FIRST_ID - 1;

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
		factory.initialize(Byte.MAX_VALUE);

		thrown.expect(IdsFactoryException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("amount of available identifiers is reached"));

		factory.getId();
	}

	/**
	 * Tests the implementation of {@link ByteIdsFactory#setIdAsUsed(Byte)}.
	 */
	@Test
	public void testSetUsedId() {
		factory.initialize(ByteIdsFactory.FIRST_ID);
		assertEquals(new Byte((byte) (ByteIdsFactory.FIRST_ID + 1)),
				factory.getId());

		// check the usage of the next byte
		factory.setIdAsUsed((byte) 5);
		assertEquals(new Byte((byte) 6), factory.getId());
		factory.setIdAsUsed((byte) 5);
		assertEquals(new Byte((byte) 7), factory.getId());
		factory.setIdAsUsed((byte) 7);
		assertEquals(new Byte((byte) 8), factory.getId());
		factory.setIdAsUsed((byte) 100);
		assertEquals(new Byte((byte) 101), factory.getId());

		// also the max should be setable
		factory.setIdAsUsed(Byte.MAX_VALUE);
	}
}
