package net.meisen.dissertation.model.persistence;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.exceptions.PersistorException;
import net.meisen.dissertation.help.ExceptionBasedTest;
import net.meisen.dissertation.model.IPersistable;
import net.meisen.dissertation.model.persistence.mock.MockPersistable;
import net.meisen.dissertation.model.persistence.mock.MockPersistor;
import net.meisen.general.sbconfigurator.api.IConfiguration;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the implementation of a {@code BasePersistor}.
 * 
 * @author pmeisen
 * 
 * @see BasePersistor
 * 
 */
@RunWith(JUnitConfigurationRunner.class)
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
@SystemProperty(property = "testBeans.selector", value = "?")
public class TestBasePersistor extends ExceptionBasedTest {

	@Autowired(required = true)
	@Qualifier("coreConfiguration")
	private IConfiguration configuration;

	private BasePersistor persistor;

	/**
	 * Initializes a spy of a {@code BasePersistor}, which is based on the
	 * {@link MockPersistor}.
	 */
	@Before
	public void init() {
		persistor = spy(configuration.createInstance(MockPersistor.class));
	}

	/**
	 * Tests the implicit call of
	 * {@link IPersistable#isRegistered(BasePersistor, Group)} which has to be
	 * triggered by the {@code BasePersistor} whenever an instance is
	 * registered, i.e. {@link BasePersistor#register(Group, IPersistable)} is
	 * called.
	 */
	@Test
	public void testIsRegisteredCall() {
		final Group group = new Group("my", "group");
		final IPersistable persistable = spy(configuration
				.createInstance(MockPersistable.class));

		// execute the method
		persistor.register(group, persistable);

		// make sure the methods are called correctly
		verify(persistor, times(1)).register(group, persistable);
		verify(persistable, times(1)).isRegistered(persistor, group);
	}

	/**
	 * Tests the registration of a {@code Persistable}.
	 */
	@Test
	public void testRegistration() {
		final Group group = new Group("MyGroup");
		final IPersistable persistable = new MockPersistable();

		// register the same persistable several times, that should work
		persistor.register(group, persistable);
		persistor.register(group, persistable);
		persistor.register(group, persistable);
		persistor.register(group, persistable);
	}

	/**
	 * Tests the implementation of {@link BasePersistor#getPersistable(Group)}.
	 */
	@Test
	public void testGetPersistable() {
		final Group group = new Group();
		final IPersistable persistable = new MockPersistable();

		// execute the method
		persistor.register(group, persistable);
		assertEquals(persistable, persistor.getPersistable(group));
		assertNull(persistor.getPersistable(null));
	}

	/**
	 * Tests the exception to be thrown when {@code null} is used as group for
	 * registration.
	 */
	@Test
	public void testExceptionWhenGroupNullRegistration() {
		thrown.expect(PersistorException.class);
		thrown.expectMessage("You have to specify a group when registering.");

		final IPersistable persistable = new MockPersistable();

		// execute the method
		persistor.register(null, persistable);
	}

	/**
	 * Tests the exception to be thrown when {@code null} persistable is
	 * registered.
	 */
	@Test
	public void testExceptionWhenPersistableNullRegistration() {
		thrown.expect(PersistorException.class);
		thrown.expectMessage("Please specify which persistable to be registered.");

		// execute the method
		persistor.register(new Group("MyGroup"), null);
	}

	/**
	 * Tests when different persistables are registered for the same group.
	 */
	@Test
	public void testExceptionWhenDoubleRegistration() {
		thrown.expect(PersistorException.class);
		thrown.expectMessage("Cannot register several persistables for the group 'MyGroup'.");

		final Group group = new Group("MyGroup");

		// execute the method
		persistor.register(group, new MockPersistable());
		persistor.register(group, new MockPersistable());
	}
}
