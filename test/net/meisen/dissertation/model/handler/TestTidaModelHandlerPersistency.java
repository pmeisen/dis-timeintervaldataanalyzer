package net.meisen.dissertation.model.handler;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Set;

import net.meisen.dissertation.server.TidaServer;

import org.junit.After;
import org.junit.Test;

/**
 * Tests the {@code TidaModelHandler} considering the knowledge about the
 * loaded, available and auto-loaded models.
 * 
 * @author pmeisen
 * 
 */
public class TestTidaModelHandlerPersistency {

	private TidaServer server;

	/**
	 * Cleans up after a test failed.
	 */
	@After
	public void cleanUp() {
		if (server != null && server.isRunning()) {
			server.shutdown(true);
		}
	}

	/**
	 * Tests the loading, unloading and dropping of models. Especially the
	 * implementations:
	 * <ul>
	 * <li>{@link TidaModelHandler#getTidaModels()}</li>
	 * <li>{@link TidaModelHandler#getAutoloadedTidaModels()}</li>
	 * <li>{@link TidaModelHandler#getAvailableTidaModels()}</li>
	 * </ul>
	 * are tested.
	 */
	@Test
	public void testLoadUnloadAndDrop() {
		Set<String> set;

		// let's create a server instance
		server = TidaServer.create();

		// start the server, load some models for the server and shut it down
		server.startAsync();
		server.fireQuery("LOAD FROM 'classpath://net/meisen/dissertation/model/handler/config/testModel1.xml' SET AUTOLOAD=true");
		server.fireQuery("LOAD FROM 'classpath://net/meisen/dissertation/model/handler/config/testModel2.xml' SET AUTOLOAD=false");
		server.fireQuery("LOAD FROM 'classpath://net/meisen/dissertation/model/handler/config/testModelModLocation1.xml' SET AUTOLOAD=true");
		server.fireQuery("LOAD FROM 'classpath://net/meisen/dissertation/model/handler/config/testModelModLocation2.xml' SET AUTOLOAD=false");

		set = server.getAutoloadedTidaModels();
		assertEquals(2, set.size());
		assertTrue(set.contains("testModel1"));
		assertFalse(set.contains("testModel2"));
		assertTrue(set.contains("testModelModLocation1"));
		assertFalse(set.contains("testModelModLocation2"));

		set = server.getTidaModels();
		assertEquals(4, set.size());
		assertTrue(set.contains("testModel1"));
		assertTrue(set.contains("testModel2"));
		assertTrue(set.contains("testModelModLocation1"));
		assertTrue(set.contains("testModelModLocation2"));

		set = server.getAvailableTidaModels();
		assertEquals(4, set.size());
		assertTrue(set.contains("testModel1"));
		assertTrue(set.contains("testModel2"));
		assertTrue(set.contains("testModelModLocation1"));
		assertTrue(set.contains("testModelModLocation2"));

		// UNLOAD testModel1 again
		server.fireQuery("UNLOAD testModel1");
		set = server.getAutoloadedTidaModels();
		assertEquals(2, set.size());
		assertTrue(set.contains("testModel1"));
		assertFalse(set.contains("testModel2"));
		assertTrue(set.contains("testModelModLocation1"));
		assertFalse(set.contains("testModelModLocation2"));

		set = server.getTidaModels();
		assertEquals(3, set.size());
		assertFalse(set.contains("testModel1"));
		assertTrue(set.contains("testModel2"));
		assertTrue(set.contains("testModelModLocation1"));
		assertTrue(set.contains("testModelModLocation2"));

		set = server.getAvailableTidaModels();
		assertEquals(4, set.size());
		assertTrue(set.contains("testModel1"));
		assertTrue(set.contains("testModel2"));
		assertTrue(set.contains("testModelModLocation1"));
		assertTrue(set.contains("testModelModLocation2"));

		// LOAD testModel1 again
		server.fireQuery("LOAD testModel1");
		set = server.getAutoloadedTidaModels();
		assertEquals(2, set.size());
		assertTrue(set.contains("testModel1"));
		assertFalse(set.contains("testModel2"));
		assertTrue(set.contains("testModelModLocation1"));
		assertFalse(set.contains("testModelModLocation2"));

		set = server.getTidaModels();
		assertEquals(4, set.size());
		assertTrue(set.contains("testModel1"));
		assertTrue(set.contains("testModel2"));
		assertTrue(set.contains("testModelModLocation1"));
		assertTrue(set.contains("testModelModLocation2"));

		set = server.getAvailableTidaModels();
		assertEquals(4, set.size());
		assertTrue(set.contains("testModel1"));
		assertTrue(set.contains("testModel2"));
		assertTrue(set.contains("testModelModLocation1"));
		assertTrue(set.contains("testModelModLocation2"));

		server.shutdown();
		assertFalse(server.isRunning());

		// re-start the server and validate
		server = TidaServer.create();
		server.startAsync();

		set = server.getAutoloadedTidaModels();
		assertEquals(2, set.size());
		assertTrue(set.contains("testModel1"));
		assertFalse(set.contains("testModel2"));
		assertTrue(set.contains("testModelModLocation1"));
		assertFalse(set.contains("testModelModLocation2"));

		set = server.getTidaModels();
		assertEquals(2, set.size());
		assertTrue(set.contains("testModel1"));
		assertFalse(set.contains("testModel2"));
		assertTrue(set.contains("testModelModLocation1"));
		assertFalse(set.contains("testModelModLocation2"));

		set = server.getAvailableTidaModels();
		assertEquals(4, set.size());
		assertTrue(set.contains("testModel1"));
		assertTrue(set.contains("testModel2"));
		assertTrue(set.contains("testModelModLocation1"));
		assertTrue(set.contains("testModelModLocation2"));

		// try to load another one
		server.fireQuery("LOAD testModelModLocation2");
		set = server.getAutoloadedTidaModels();
		assertEquals(2, set.size());
		assertTrue(set.contains("testModel1"));
		assertFalse(set.contains("testModel2"));
		assertTrue(set.contains("testModelModLocation1"));
		assertFalse(set.contains("testModelModLocation2"));

		set = server.getTidaModels();
		assertEquals(3, set.size());
		assertTrue(set.contains("testModel1"));
		assertFalse(set.contains("testModel2"));
		assertTrue(set.contains("testModelModLocation1"));
		assertTrue(set.contains("testModelModLocation2"));

		set = server.getAvailableTidaModels();
		assertEquals(4, set.size());
		assertTrue(set.contains("testModel1"));
		assertTrue(set.contains("testModel2"));
		assertTrue(set.contains("testModelModLocation1"));
		assertTrue(set.contains("testModelModLocation2"));

		// delete a model
		server.fireQuery("DROP MODEL testModelModLocation2");
		set = server.getAutoloadedTidaModels();
		assertEquals(2, set.size());
		assertTrue(set.contains("testModel1"));
		assertFalse(set.contains("testModel2"));
		assertTrue(set.contains("testModelModLocation1"));
		assertFalse(set.contains("testModelModLocation2"));

		set = server.getTidaModels();
		assertEquals(2, set.size());
		assertTrue(set.contains("testModel1"));
		assertFalse(set.contains("testModel2"));
		assertTrue(set.contains("testModelModLocation1"));
		assertFalse(set.contains("testModelModLocation2"));

		set = server.getAvailableTidaModels();
		assertEquals(3, set.size());
		assertTrue(set.contains("testModel1"));
		assertTrue(set.contains("testModel2"));
		assertTrue(set.contains("testModelModLocation1"));
		assertFalse(set.contains("testModelModLocation2"));

		server.fireQuery("DROP MODEL testModel2");
		set = server.getAutoloadedTidaModels();
		assertEquals(2, set.size());
		assertTrue(set.contains("testModel1"));
		assertFalse(set.contains("testModel2"));
		assertTrue(set.contains("testModelModLocation1"));
		assertFalse(set.contains("testModelModLocation2"));

		set = server.getTidaModels();
		assertEquals(2, set.size());
		assertTrue(set.contains("testModel1"));
		assertFalse(set.contains("testModel2"));
		assertTrue(set.contains("testModelModLocation1"));
		assertFalse(set.contains("testModelModLocation2"));

		set = server.getAvailableTidaModels();
		assertEquals(2, set.size());
		assertTrue(set.contains("testModel1"));
		assertFalse(set.contains("testModel2"));
		assertTrue(set.contains("testModelModLocation1"));
		assertFalse(set.contains("testModelModLocation2"));

		server.shutdown(true);
		assertFalse(server.isRunning());
	}

	/**
	 * Tests the loading of an invalid model, i.e. the model fails during
	 * spring-loading.
	 */
	@Test
	public void testInvalidLoading() {
		Set<String> set;

		// let's create a server instance
		server = TidaServer.create();

		// start the server, load some models for the server and shut it down
		server.startAsync();
		try {
			server.fireQuery("LOAD FROM 'classpath://net/meisen/dissertation/model/handler/config/testInvalidModel1.xml' SET AUTOLOAD=true");
		} catch (final Exception e) {
		}

		set = server.getTidaModels();
		assertEquals(0, set.size());
		assertFalse(set.contains("testInvalidModel1"));

		set = server.getAutoloadedTidaModels();
		assertEquals(0, set.size());
		assertFalse(set.contains("testInvalidModel1"));

		set = server.getAvailableTidaModels();
		assertEquals(0, set.size());
		assertFalse(set.contains("testInvalidModel1"));
	}

	/**
	 * Tests the loading of a model failing during initialisation and it's
	 * removal.
	 */
	@Test
	public void testFailedLoading() {
		Set<String> set;

		// let's create a server instance
		server = TidaServer.create();

		// start the server, load some models for the server and shut it down
		server.startAsync();
		try {
			server.fireQuery("LOAD FROM 'classpath://net/meisen/dissertation/model/handler/config/testFailedModel1.xml' SET AUTOLOAD=true");
		} catch (final Exception e) {
		}

		set = server.getTidaModels();
		assertEquals(0, set.size());
		assertFalse(set.contains("testFailedModel1"));

		set = server.getAutoloadedTidaModels();
		assertEquals(0, set.size());
		assertFalse(set.contains("testFailedModel1"));

		set = server.getAvailableTidaModels();
		assertEquals(0, set.size());
		assertFalse(set.contains("testFailedModel1"));
	}
}
