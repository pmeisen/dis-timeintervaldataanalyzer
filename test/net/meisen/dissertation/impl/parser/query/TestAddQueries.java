package net.meisen.dissertation.impl.parser.query;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.text.ParseException;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.impl.auth.shiro.ShiroAuthManager;
import net.meisen.dissertation.impl.parser.query.add.AddQuery;
import net.meisen.dissertation.impl.parser.query.add.AddResult;
import net.meisen.dissertation.impl.parser.query.add.AddType;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
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
 * Tests the implementation of {@link AddQuery}.
 * 
 * @author pmeisen
 * 
 */
@SystemProperties(value = {
		@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/impl/parser/query/tidaConfigUsingShiroAuth.xml"),
		@SystemProperty(property = "test.tmpFolder", value = "testAddQuery") })
public class TestAddQueries extends LoaderBasedTest {
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
	 * Tests the parsing of a {@code AddQuery} for a user.
	 * 
	 * @throws ParseException
	 *             if the parsing fails
	 */
	@Test
	public void testAddUserQueryParsing() throws ParseException {
		AddQuery q;

		q = q("ADD USER 'philipp' WITH PASSWORD 'password'");
		assertEquals(AddType.USER, q.getEntityType());
		assertEquals("philipp", q.getEntityName());
		assertEquals("password", q.getEntityPassword());
		assertEquals(0, q.getPermissions().size());
		assertEquals(0, q.getRoles().size());

		q = q("ADD USER 'philipp' WITH PASSWORD 'password' WITH PERMISSIONS 'GLOBAL.get' WITH ROLES 'admin'");
		assertEquals(AddType.USER, q.getEntityType());
		assertEquals("philipp", q.getEntityName());
		assertEquals("password", q.getEntityPassword());
		assertEquals(1, q.getPermissions().size());
		assertEquals(1, q.getRoles().size());
		assertTrue(q.getPermissions().contains(Permission.get.create()));
		assertTrue(q.getRoles().contains("admin"));

		q = q("ADD USER 'philipp' WITH PASSWORD 'password' WITH ROLES 'admin', 'user' WITH PERMISSIONS 'GLOBAL.queryAll', 'MODEL.myModel.query'");
		assertEquals(AddType.USER, q.getEntityType());
		assertEquals("philipp", q.getEntityName());
		assertEquals("password", q.getEntityPassword());
		assertEquals(2, q.getPermissions().size());
		assertEquals(2, q.getRoles().size());
		assertTrue(q.getPermissions().contains(Permission.queryAll.create()));
		assertTrue(q.getPermissions().contains(
				Permission.query.create("myModel")));
		assertTrue(q.getRoles().contains("admin"));
		assertTrue(q.getRoles().contains("user"));
	}

	/**
	 * Tests the parsing of a {@code AddQuery} for roles.
	 * 
	 * @throws ParseException
	 *             if the parsing fails
	 */
	@Test
	public void testAddRoleQueryParsing() throws ParseException {
		boolean exception;
		AddQuery q;

		try {
			exception = false;
			q = q("ADD ROLE 'ROLE' WITH PASSWORD 'password'");
		} catch (final Exception e) {
			exception = true;
		}
		assertTrue(exception);

		try {
			exception = false;
			q = q("ADD ROLE 'ROLE' WITH ROLES 'admin'");
		} catch (final Exception e) {
			exception = true;
		}
		assertTrue(exception);

		try {
			exception = false;
			q = q("ADD ROLE 'ROLE' WITH PERMISSIONS 'GLOBAL.doNotHave'");
		} catch (final Exception e) {
			exception = true;
		}
		assertTrue(exception);

		q = q("ADD ROLE 'role'");
		assertEquals(AddType.ROLE, q.getEntityType());
		assertEquals("role", q.getEntityName());
		assertNull(q.getEntityPassword());
		assertEquals(0, q.getPermissions().size());
		assertEquals(0, q.getRoles().size());

		q = q("ADD ROLE 'role' WITH PERMISSIONS 'GLOBAL.queryAll', 'MODEL.myModel.query'");
		assertEquals(AddType.ROLE, q.getEntityType());
		assertEquals("role", q.getEntityName());
		assertNull(q.getEntityPassword());
		assertEquals(2, q.getPermissions().size());
		assertEquals(0, q.getRoles().size());
		assertTrue(q.getPermissions().contains(Permission.queryAll.create()));
		assertTrue(q.getPermissions().contains(
				Permission.query.create("myModel")));
	}

	/**
	 * Tests the evaluation of a {@code AddQuery}.
	 */
	@Test
	public void testAddQueryEvaluation() {
		String query;

		// fire the query and get the result
		authManager.login("admin", "password");
		query = "ADD ROLE 'newRole'";
		assertNotNull((AddResult) factory.evaluateQuery(q(query), null));

		// no exception means the role was really created
		authManager.grantPermissionsToRole("newRole",
				new DefinedPermission[] { Permission.connectHTTP.create() });

		// add a new user and log in as the new user
		query = "ADD USER 'user1' WITH PASSWORD 'user1_password'";
		assertNotNull((AddResult) factory.evaluateQuery(q(query), null));
		authManager.logout();
		authManager.login("user1", "user1_password");
		authManager.logout();
		authManager.login("admin", "password");

		// create a new role and assign it to the previously created user
		query = "ADD ROLE 'testRole1' WITH PERMISSIONS 'GLOBAL.queryAll', 'MODEL.myModel.query'";
		assertNotNull((AddResult) factory.evaluateQuery(q(query), null));
		authManager.assignRoleToUser("user1", "testRole1");
		authManager.logout();
		authManager.login("user1", "user1_password");
		assertTrue(authManager.hasPermission(Permission.queryAll.create()));
		assertTrue(authManager
				.hasPermission(Permission.query.create("myModel")));
		assertFalse(authManager.hasPermission(Permission.query
				.create("myAnotherModel")));
		authManager.logout();
		authManager.login("admin", "password");

		// create a user with permissions
		query = "ADD USER 'user2' WITH PASSWORD 'user2_password' WITH PERMISSIONS 'GLOBAL.manageUsers', 'MODEL.anotherModel.query'";
		assertNotNull((AddResult) factory.evaluateQuery(q(query), null));
		authManager.logout();
		authManager.login("user2", "user2_password");
		assertTrue(authManager.hasPermission(Permission.manageUsers.create()));
		assertTrue(authManager.hasPermission(Permission.query
				.create("anotherModel")));
		authManager.logout();
		authManager.login("admin", "password");

		// create user and role
		query = "ADD USER 'user3' WITH PASSWORD 'user3_password' WITH ROLES 'testRole2' WITH PERMISSIONS 'MODEL.adm.query'";
		assertNotNull((AddResult) factory.evaluateQuery(q(query), null));
		query = "ADD ROLE 'testRole2' WITH PERMISSIONS 'MODEL.adm.modify', 'GLOBAL.get'";
		assertNotNull((AddResult) factory.evaluateQuery(q(query), null));
		authManager.logout();
		authManager.login("user3", "user3_password");
		assertTrue(authManager.hasPermission(Permission.query.create("adm")));
		assertTrue(authManager.hasPermission(Permission.modify.create("adm")));
		assertTrue(authManager.hasPermission(Permission.get.create()));
		assertFalse(authManager.hasPermission(Permission.connectHTTP.create()));
		assertFalse(authManager.hasPermission(Permission.connectTSQL.create()));
		authManager.logout();
		authManager.login("admin", "password");

		// cleanUp in the end
		authManager.logout();
		authManager.release();
	}
}
