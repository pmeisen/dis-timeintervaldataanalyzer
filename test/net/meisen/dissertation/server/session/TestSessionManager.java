package net.meisen.dissertation.server.session;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.server.sessions.SessionManager;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.After;
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
			manager.cleanUp();
		}

		/**
		 * Tests the creation of a temporary directory.
		 */
		@Test
		public void testCreateTmpDir() {
			final File tmpDir = manager.getTempDir();
			assertTrue(tmpDir.exists());
			manager.cleanUp();
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

			manager.cleanUp();
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

			manager.cleanUp();
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
			manager.cleanUp();
			assertFalse(tmpDir.exists());
		}

		/**
		 * Make sure it's cleaned up
		 */
		@After
		public void reset() {
			manager.cleanUp();
		}
	}

	/**
	 * The test suite for the {@code SessionManager} tests.
	 * 
	 * @author pmeisen
	 * 
	 */
	@RunWith(Suite.class)
	@Suite.SuiteClasses({ TestDefaultSessionManager.class,
			TestChangedTmpRoot.class })
	public static class TestSessionManagerSuite {
		// just the suite with all the tests defined here
	}
}
