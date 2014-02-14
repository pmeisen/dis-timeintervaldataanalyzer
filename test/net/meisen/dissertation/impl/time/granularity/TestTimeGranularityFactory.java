package net.meisen.dissertation.impl.time.granularity;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.exceptions.TimeGranularityFactoryException;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.impl.time.granularity.mock.MockInvalidTimeGranularity;
import net.meisen.dissertation.impl.time.granularity.mock.MockValidTimeGranularityByConstructor;
import net.meisen.dissertation.impl.time.granularity.mock.MockValidTimeGranularityByInstance;
import net.meisen.dissertation.model.time.granularity.CentiSecond;
import net.meisen.dissertation.model.time.granularity.Day;
import net.meisen.dissertation.model.time.granularity.Hour;
import net.meisen.dissertation.model.time.granularity.ITimeGranularity;
import net.meisen.dissertation.model.time.granularity.MilliSecond;
import net.meisen.dissertation.model.time.granularity.Minute;
import net.meisen.dissertation.model.time.granularity.Second;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.Before;
import org.junit.Test;

/**
 * Tests the implemenation of {@link TimeGranularityFactory}.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
public class TestTimeGranularityFactory extends ModuleBasedTest {

	private TimeGranularityFactory factory;

	/**
	 * Create the factory to be tested.
	 */
	@Before
	public void setUp() {
		factory = new TimeGranularityFactory();
		configuration.wireInstance(factory);
	}

	/**
	 * Tests the implementation of
	 * {@link TimeGranularityFactory#findClass(String)}.
	 */
	@Test
	public void testFindClassByClassName() {
		assertEquals(Minute.class, factory.findClass(Minute.class.getName()));
		assertEquals(Second.class, factory.findClass(Second.class.getName()));
		assertEquals(Hour.class, factory.findClass(Hour.class.getName()));
		assertEquals(MilliSecond.class,
				factory.findClass(MilliSecond.class.getName()));
	}

	/**
	 * Tests the implementation of {@link TimeGranularityFactory#find(String)}.
	 */
	@Test
	public void testFindByClassName() {
		assertTrue(Minute.instance() == factory.find(Minute.class.getName()));
		assertTrue(Second.instance() == factory.find(Second.class.getName()));
		assertTrue(Day.instance().equals(factory.find(Day.class.getName())));
	}

	/**
	 * Tests the usage of {@link TimeGranularityFactory#findClass(String)} using
	 * short names.
	 */
	@Test
	public void testFindClassByShortName() {
		assertEquals(Second.class, factory.findClass("Second"));
		assertEquals(Hour.class, factory.findClass("HOUR"));
		assertEquals(MilliSecond.class, factory.findClass("MILLISECOND"));
		assertEquals(CentiSecond.class, factory.findClass("Centisecond"));
	}

	/**
	 * Tests the implementation of {@link TimeGranularityFactory#find(String)}
	 * using short names.
	 */
	@Test
	public void testFindByShortName() {
		assertTrue(Minute.instance() == factory.find("Minute"));
		assertTrue(Second.instance() == factory.find("SECOND"));
		assertTrue(Day.instance().equals(factory.find("dAy")));
	}

	/**
	 * Tests the additional implementation of a {@code TimeGranularity} using a
	 * valid constructor.
	 */
	@Test
	public void testCreationByConstructor() {
		final ITimeGranularity g = factory
				.find(MockValidTimeGranularityByConstructor.class.getName());
		assertTrue(g instanceof MockValidTimeGranularityByConstructor);
	}

	/**
	 * Tests the additional implementation of a {@code TimeGranularity} using an
	 * instance-method.
	 */
	@Test
	public void testCreationByInstance() {
		final ITimeGranularity g = factory
				.find(MockValidTimeGranularityByInstance.class.getName());
		assertTrue(g instanceof MockValidTimeGranularityByInstance);
	}

	/**
	 * Checks if an exception is thrown when {@code null} is used as name.
	 */
	@Test
	public void testExceptionWhenNullName() {
		thrown.expect(TimeGranularityFactoryException.class);
		thrown.expectMessage("name of the 'TimeGranularity' to be created cannot be 'null'");

		factory.find(null);
	}

	/**
	 * Checks if an exception is thrown when an invalid name is used.
	 */
	@Test
	public void testExceptionWhenInvalidName() {
		thrown.expect(TimeGranularityFactoryException.class);
		thrown.expectMessage("Cannot find any valid implementation with the name 'I-Will-Never-Exist'");

		factory.find("I-Will-Never-Exist");
	}

	/**
	 * Checks if an exception is thrown when an invalid implementation is used.
	 */
	@Test
	public void testExceptionForInvalidImplementation() {
		thrown.expect(TimeGranularityFactoryException.class);
		thrown.expectMessage("Could not find any default-constructor nor any 'instance'-methode for the TimeGranularity-implementation '"
				+ MockInvalidTimeGranularity.class.getName() + "'");

		factory.find(MockInvalidTimeGranularity.class.getName());
	}
}
