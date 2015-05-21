package net.meisen.dissertation.server.session;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.SessionManagerException;
import net.meisen.dissertation.help.ExceptionBasedTest;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.server.sessions.Session;
import net.meisen.dissertation.server.sessions.SessionManager;
import net.meisen.general.genmisc.exceptions.catalog.DefaultLocalizedExceptionCatalog;
import net.meisen.general.genmisc.exceptions.registry.DefaultExceptionRegistry;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.genmisc.types.Streams;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests for the {@code SessionManager}.
 * 
 * @author pmeisen
 * 
 */
public class TestSessionManager extends LoaderBasedTest {

	/**
	 * Tests the resolve functionality of the {@code SessionManager}.
	 * 
	 * @author pmeisen
	 * 
	 */
	public static class TestResolveFunctionality extends ExceptionBasedTest {

		private SessionManager manager;

		/**
		 * Creates a manager with exceptionRegistry.
		 */
		@Before
		public void createManager() {
			manager = new SessionManager();
			final DefaultExceptionRegistry registry = new DefaultExceptionRegistry();
			registry.addExceptionCatalogByClass(SessionManagerException.class,
					DefaultLocalizedExceptionCatalog.class);
			manager.setExceptionRegistry(registry);
		}

		/**
		 * Cleans up afterwards.
		 */
		@After
		public void cleanUp() {
			manager.release();
			manager = null;
		}

		/**
		 * Tests the usage of an invalid {@code null} {@code URI}.
		 */
		@Test
		public void testInvalidNullUri() {
			thrown.expect(SessionManagerException.class);
			thrown.expectMessage("the identifier is invalid");
			manager.resolve(null);
		}

		/**
		 * Tests the usage of an invalid empty {@code URI}.
		 */
		@Test
		public void testInvalidEmptyUri() {
			thrown.expect(SessionManagerException.class);
			thrown.expectMessage("the identifier is invalid");
			manager.resolve("");
		}

		/**
		 * Tests the usage of an invalid whitespace {@code URI}.
		 */
		@Test
		public void testInvalidWhitespaceUri() {
			thrown.expect(SessionManagerException.class);
			thrown.expectMessage("the identifier is invalid");
			manager.resolve("  ");
		}

		/**
		 * Tests the usage of an invalid {@code URI}, considering the
		 * {@code URI} syntax.
		 */
		@Test
		public void testInvalidUri() {
			thrown.expect(SessionManagerException.class);
			thrown.expectMessage("the identifier is invalid");
			manager.resolve("\\uploaded/sessionId/file");
		}

		/**
		 * Tests the usage of an invalid protocol.
		 */
		@Test
		public void testInvalidProtocol() {
			thrown.expect(SessionManagerException.class);
			thrown.expectMessage("protocol 'invalid' is not supported");
			manager.resolve("invalid://sessionId/file");
		}

		/**
		 * Tests the usage of an invalid file specification.
		 */
		@Test
		public void testInvalidFile() {
			thrown.expect(SessionManagerException.class);
			thrown.expectMessage("file 'file/subdir' is not supported");
			manager.resolve("uploaded://sessionId/file/subdir");
		}

		/**
		 * Tests the usage of an invalid session.
		 */
		@Test
		public void testInvalidSession() {
			thrown.expect(SessionManagerException.class);
			thrown.expectMessage("session with id 'sessionId' could not be found");
			manager.resolve("uploaded://sessionId/file");
		}

		/**
		 * Test the resolving of a resource.
		 * 
		 * @throws IOException
		 *             if the test-file cannot be created
		 */
		@Test
		public void testResourceResolving() throws IOException {
			final Session session = manager.createSession("philipp");
			final File dir = manager.getSessionDir(session.getId(), true);
			final File file = new File(dir, "lala");
			assertTrue(file.createNewFile());

			// check the resource
			final InputStream is = manager.resolve("uploaded://"
					+ session.getId() + "/lala");
			assertNotNull(is);
			Streams.closeIO(is);
			manager.release();
		}
	}

	/**
	 * Tests the default configuration of the manager.
	 * 
	 * @author pmeisen
	 * 
	 */
	public static class TestDefaultSessionManager extends LoaderBasedTest {

		@Autowired
		@Qualifier(DefaultValues.SESSIONMANAGER_ID)
		private SessionManager manager;

		/**
		 * Resets the directory and the manager.
		 */
		@After
		public void reset() {
			manager.resetTempDir();
			manager.release();
		}

		/**
		 * Tests the creation of a temporary directory.
		 */
		@Test
		public void testCreateTmpDir() {
			final File tmpDir = manager.getTempDir();
			assertTrue(tmpDir.exists());
			manager.release();
			assertFalse(tmpDir.exists());
		}

		/**
		 * Tests the modification of the temporary directory.
		 */
		@Test
		public void testModifyTmpDir() {
			final File defTmpDir = manager.getTempDir();
			assertTrue(defTmpDir.exists());

			final File newTmpDir = new File(
					System.getProperty("java.io.tmpdir"), "myNewDir");
			manager.setTmpDir(Files.getCanonicalPath(newTmpDir));
			assertFalse(defTmpDir.exists());
			assertEquals(manager.getTempDir(), newTmpDir);

			manager.release();
			assertFalse(newTmpDir.exists());
		}

		/**
		 * Tests the creation and removal of a folder for a specific session.
		 */
		@Test
		public void testSessionFolderCreation() {
			String sessionId;
			File sessionDir;

			// remove the session
			sessionId = manager.createSession("philipp").getId();
			sessionDir = manager.getSessionDir(sessionId, true);
			assertNotNull(sessionDir);
			assertEquals(new File(manager.getTempDir(), sessionId), sessionDir);
			assertTrue(manager.getTempDir().exists());
			assertTrue(sessionDir.exists());

			manager.removeSession(sessionId);
			assertFalse(sessionDir.exists());

			// create another one and let it be deleted by the manager
			sessionId = manager.createSession("philipp").getId();
			sessionDir = manager.getSessionDir(sessionId, true);
			assertNotNull(sessionDir);
			assertEquals(new File(manager.getTempDir(), sessionId), sessionDir);
			assertTrue(sessionDir.exists());

			manager.release();
			assertFalse(sessionDir.exists());
		}
	}

	/**
	 * Tests the configuration based changing of a directory.
	 * 
	 * @author pmeisen
	 * 
	 */
	@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/server/session/testTmpRootConfig.xml")
	public static class TestChangedTmpRoot extends LoaderBasedTest {

		@Autowired
		@Qualifier(DefaultValues.SESSIONMANAGER_ID)
		private SessionManager manager;

		/**
		 * Check if the directory was really changed.
		 */
		@Test
		public void testChangedDirectory() {
			final File tmpDir = manager.getTempDir();
			assertTrue(tmpDir.exists());
			assertEquals(tmpDir.getParentFile(),
					new File(System.getProperty("java.io.tmpdir")));
			manager.release();
			assertFalse(tmpDir.exists());
		}

		/**
		 * Make sure it's cleaned up
		 */
		@After
		public void reset() {
			manager.release();
		}
	}

	/**
	 * The test suite for the {@code SessionManager} tests.
	 * 
	 * @author pmeisen
	 * 
	 */
	@RunWith(Suite.class)
	@Suite.SuiteClasses({ TestResolveFunctionality.class,
			TestDefaultSessionManager.class, TestChangedTmpRoot.class })
	public static class TestSessionManagerSuite {
		// just the suite with all the tests defined here
	}
}
