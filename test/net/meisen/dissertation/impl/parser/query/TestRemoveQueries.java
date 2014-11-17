package net.meisen.dissertation.impl.parser.query;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.text.ParseException;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.impl.auth.shiro.ShiroAuthManager;
import net.meisen.dissertation.impl.parser.query.remove.RemoveQuery;
import net.meisen.dissertation.impl.parser.query.remove.RemoveResult;
import net.meisen.dissertation.model.auth.permissions.Permission;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperties;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the implementation of {@link RemoveQuery}.
 * 
 * @author pmeisen
 * 
 */
@SystemProperties(value = {
		@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/impl/parser/query/tidaConfigUsingShiroAuth.xml"),
		@SystemProperty(property = "test.tmpFolder", value = "testRemoveQuery") })
public class TestRemoveQueries extends LoaderBasedTest {
	private static TidaModelHandler h = null;

	@Autowired
	@Qualifier(DefaultValues.MODELHANDLER_ID)
	private TidaModelHandler handler;

	@Autowired
	@Qualifier(DefaultValues.QUERYFACTORY_ID)
	private QueryFactory factory;

	@Autowired
	@Qualifier(DefaultValues.AUTHMANAGER_ID)
	private ShiroAuthManager authManager;

	/**
	 * Get the factory's result for the {@code query}.
	 * 
	 * @param query
	 *            the query to be created
	 * @return the created {@code Query}
	 */
	@SuppressWarnings("unchecked")
	protected <T extends IQuery> T q(final String query) {
		return (T) factory.parseQuery(query);
	}

	/**
	 * Cleans up by trying to delete created files.
	 */
	@BeforeClass
	@AfterClass
	public static void cleanup() {
		if (h == null) {
			Files.deleteOnExitDir(new File(
					System.getProperty("java.io.tmpdir"), "testRemoveQuery"));
		} else {
			h.unloadAll();
			Files.deleteOnExitDir(new File(h.getDefaultLocation()));
		}
	}

	/**
	 * Initializes the tests by setting the static handler {@code h}.
	 */
	@Before
	public void set() {

		if (h == null) {
			h = handler;
		} else {
			assertTrue(h == handler);
		}
	}

	/**
	 * Tests the parsing of a {@code AssignQuery}.
	 * 
	 * @throws ParseException
	 *             if the parsing fails
	 */
	@Test
	public void testRemoveQueryParsing() throws ParseException {
		RemoveQuery q;

		q = q("REMOVE ROLE 'role' FROM USER 'philipp'");
		assertEquals("philipp", q.getEntityName());
		assertEquals(1, q.getRoles().size());
		assertTrue(q.getRoles().contains("role"));

		q = q("REMOVE ROLES 'role1', 'role2', 'role3' FROM USER 'philipp'");
		assertEquals("philipp", q.getEntityName());
		assertEquals(3, q.getRoles().size());
		assertTrue(q.getRoles().contains("role1"));
		assertTrue(q.getRoles().contains("role2"));
		assertTrue(q.getRoles().contains("role3"));
	}

	/**
	 * Tests the evaluation of a {@code RemoveQuery}.
	 */
	@Test
	public void testRemoveQueryEvaluation() {
		String query;

		// fire the query and get the result
		authManager.login("admin", "password");
		factory.evaluateQuery(q("ADD USER 'philipp' WITH PASSWORD 'password'"),
				null);
		factory.evaluateQuery(
				q("ADD ROLE 'role1' WITH PERMISSIONS 'GLOBAL.get', 'GLOBAL.load'"),
				null);
		factory.evaluateQuery(
				q("ADD ROLE 'role2' WITH PERMISSIONS 'MODEL.myModel.query', 'MODEL.myOtherModel.query'"),
				null);
		factory.evaluateQuery(
				q("ADD ROLE 'role3' WITH PERMISSIONS 'GLOBAL.connectTSQL'"),
				null);
		factory.evaluateQuery(
				q("ASSIGN ROLES 'role1', 'role2', 'role3' TO USER 'philipp'"),
				null);

		// check pre-requirements
		authManager.logout();
		authManager.login("philipp", "password");
		assertTrue(authManager.hasPermission(Permission.get.create()));
		assertTrue(authManager.hasPermission(Permission.load.create()));
		assertTrue(authManager.hasPermission(Permission.connectTSQL.create()));
		assertTrue(authManager
				.hasPermission(Permission.query.create("myModel")));
		assertTrue(authManager.hasPermission(Permission.query
				.create("myOtherModel")));
		authManager.logout();
		authManager.login("admin", "password");

		// assign one role
		query = "REMOVE ROLE 'role1' FROM USER 'philipp'";
		assertNotNull((RemoveResult) factory.evaluateQuery(q(query), null));
		authManager.logout();
		authManager.login("philipp", "password");
		assertFalse(authManager.hasPermission(Permission.get.create()));
		assertFalse(authManager.hasPermission(Permission.load.create()));
		assertTrue(authManager.hasPermission(Permission.connectTSQL.create()));
		assertTrue(authManager
				.hasPermission(Permission.query.create("myModel")));
		assertTrue(authManager.hasPermission(Permission.query
				.create("myOtherModel")));
		authManager.logout();
		authManager.login("admin", "password");

		// assign two more roles
		query = "REMOVE ROLES 'role2', 'role3' FROM USER 'philipp'";
		assertNotNull((RemoveResult) factory.evaluateQuery(q(query), null));
		authManager.logout();
		authManager.login("philipp", "password");
		assertFalse(authManager.hasPermission(Permission.get.create()));
		assertFalse(authManager.hasPermission(Permission.load.create()));
		assertFalse(authManager.hasPermission(Permission.query
				.create("myModel")));
		assertFalse(authManager.hasPermission(Permission.query
				.create("myOtherModel")));
		assertFalse(authManager.hasPermission(Permission.connectTSQL.create()));

		// cleanUp in the end
		authManager.logout();
		authManager.release();
	}
}
