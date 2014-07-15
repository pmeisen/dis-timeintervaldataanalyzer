package net.meisen.dissertation.impl.parser.query;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.AuthException;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.impl.auth.shiro.ShiroAuthManager;
import net.meisen.dissertation.impl.parser.query.drop.DropQuery;
import net.meisen.dissertation.impl.parser.query.drop.DropResult;
import net.meisen.dissertation.impl.parser.query.drop.DropType;
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
 * Tests the implementation of the {@code DropQuery} statement.
 * 
 * @author pmeisen
 * 
 */
@SystemProperties(value = {
		@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/impl/parser/query/tidaConfigUsingShiroAuth.xml"),
		@SystemProperty(property = "test.tmpFolder", value = "testDropQuery") })
public class TestDropQueries extends LoaderBasedTest {
	private static TidaModelHandler h = null;

	@Autowired
	@Qualifier(DefaultValues.HANDLER_ID)
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
					System.getProperty("java.io.tmpdir"), "testAddQuery"));
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
	 * Tests the parsing of a drop-user-statement.
	 */
	@Test
	public void testDropUserParsing() {
		final DropQuery q = q("DROP USER 'philipp'");
		assertEquals(DropType.USER, q.getEntityType());
		assertEquals("philipp", q.getEntityName());
	}

	/**
	 * Tests the parsing of a drop-role-statement.
	 */
	@Test
	public void testDropRoleParsing() {
		final DropQuery q = q("DROP ROLE 'theRole'");
		assertEquals(DropType.ROLE, q.getEntityType());
		assertEquals("theRole", q.getEntityName());
	}

	/**
	 * Tests the evaluation of a {@code AddQuery}.
	 */
	@Test
	public void testDropQueryEvaluation() {
		boolean exception;

		String query;

		// add some stuff which can be dropped
		authManager.login("admin", "password");
		factory.evaluateQuery(q("ADD USER 'philipp' WITH PASSWORD 'password'"),
				null);
		factory.evaluateQuery(q("ADD USER 'tobias' WITH PASSWORD 'password'"),
				null);
		factory.evaluateQuery(q("ADD USER 'debbie' WITH PASSWORD 'password'"),
				null);
		factory.evaluateQuery(q("ADD USER 'eddie' WITH PASSWORD 'password'"),
				null);

		factory.evaluateQuery(q("ADD ROLE 'baby'"), null);
		factory.evaluateQuery(q("ADD ROLE 'adult'"), null);
		factory.evaluateQuery(
				q("ADD ROLE 'parent' WITH PERMISSIONS 'GLOBAL.get'"), null);

		// drop a user
		authManager.logout();
		authManager.login("philipp", "password");
		authManager.logout();
		authManager.login("admin", "password");
		query = "DROP USER 'philipp'";
		assertNotNull((DropResult) factory.evaluateQuery(q(query), null));
		authManager.logout();
		try {
			exception = false;
			authManager.login("philipp", "password");
		} catch (final AuthException e) {
			exception = true;
		}
		assertTrue(exception);
		authManager.login("admin", "password");

		// drop a role
		authManager.assignRoleToUser("tobias", "parent");
		authManager.logout();
		authManager.login("tobias", "password");
		assertTrue(authManager.hasPermission(Permission.get.create()));
		authManager.logout();
		authManager.login("admin", "password");
		query = "DROP ROLE 'parent'";
		assertNotNull((DropResult) factory.evaluateQuery(q(query), null));
		authManager.logout();
		authManager.login("tobias", "password");
		assertFalse(authManager.hasPermission(Permission.get.create()));
		authManager.logout();
		authManager.login("admin", "password");

	}
}
