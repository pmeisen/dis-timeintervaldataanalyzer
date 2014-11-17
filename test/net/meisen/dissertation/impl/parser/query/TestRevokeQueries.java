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
import net.meisen.dissertation.impl.parser.query.grant.GrantQuery;
import net.meisen.dissertation.impl.parser.query.revoke.RevokeQuery;
import net.meisen.dissertation.impl.parser.query.revoke.RevokeResult;
import net.meisen.dissertation.impl.parser.query.revoke.RevokeType;
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
 * Tests the implementation of {@link GrantQuery}.
 * 
 * @author pmeisen
 * 
 */
@SystemProperties(value = {
		@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/impl/parser/query/tidaConfigUsingShiroAuth.xml"),
		@SystemProperty(property = "test.tmpFolder", value = "testRevokeQuery") })
public class TestRevokeQueries extends LoaderBasedTest {
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
					System.getProperty("java.io.tmpdir"), "testRevokeQuery"));
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
	 * Tests the parsing of a {@code RevokeQuery} for a user.
	 * 
	 * @throws ParseException
	 *             if the parsing fails
	 */
	@Test
	public void testRevokeUserQueryParsing() throws ParseException {
		RevokeQuery q;

		q = q("REVOKE PERMISSIONS 'GLOBAL.get', 'MODEL.myModel.query', 'GLOBAL.load' FROM USER 'philipp'");
		assertEquals(RevokeType.USER, q.getEntityType());
		assertEquals("philipp", q.getEntityName());
		assertEquals(3, q.getPermissions().size());
		assertTrue(q.getPermissions().contains(Permission.get.create()));
		assertTrue(q.getPermissions().contains(Permission.load.create()));
		assertTrue(q.getPermissions().contains(
				Permission.query.create("myModel")));

		q = q("REVOKE PERMISSIONS 'GLOBAL.get' FROM USER 'philipp'");
		assertEquals(RevokeType.USER, q.getEntityType());
		assertEquals("philipp", q.getEntityName());
		assertEquals(1, q.getPermissions().size());
		assertTrue(q.getPermissions().contains(Permission.get.create()));
	}

	/**
	 * Tests the parsing of a {@code RevokeQuery} for roles.
	 * 
	 * @throws ParseException
	 *             if the parsing fails
	 */
	@Test
	public void testRevokeRoleQueryParsing() throws ParseException {
		RevokeQuery q;

		q = q("REVOKE PERMISSIONS 'GLOBAL.get', 'MODEL.myModel.query', 'GLOBAL.load' FROM ROLE 'sampleRole'");
		assertEquals(RevokeType.ROLE, q.getEntityType());
		assertEquals("sampleRole", q.getEntityName());
		assertEquals(3, q.getPermissions().size());
		assertTrue(q.getPermissions().contains(Permission.get.create()));
		assertTrue(q.getPermissions().contains(Permission.load.create()));
		assertTrue(q.getPermissions().contains(
				Permission.query.create("myModel")));

		q = q("REVOKE PERMISSIONS 'GLOBAL.get' FROM ROLE 'sampleRole'");
		assertEquals(RevokeType.ROLE, q.getEntityType());
		assertEquals("sampleRole", q.getEntityName());
		assertEquals(1, q.getPermissions().size());
		assertTrue(q.getPermissions().contains(Permission.get.create()));
	}

	/**
	 * Tests the evaluation of a {@code RevokeQuery}.
	 */
	@Test
	public void testRevokeQueryEvaluation() {
		String query;

		// add some things we need
		authManager.login("admin", "password");
		factory.evaluateQuery(
				q("ADD USER 'philipp' WITH PASSWORD 'password' WITH PERMISSIONS 'GLOBAL.get', 'MODEL.myModel.query', 'GLOBAL.load'"),
				null);
		factory.evaluateQuery(
				q("ADD ROLE 'adult' WITH PERMISSIONS 'GLOBAL.connectTSQL', 'MODEL.myOtherModel.query'"),
				null);
		authManager.assignRoleToUser("philipp", "adult");

		// revoke some permissions from the role
		query = "REVOKE PERMISSIONS 'GLOBAL.get', 'MODEL.myOtherModel.query' FROM ROLE 'adult'";
		assertNotNull((RevokeResult) factory.evaluateQuery(q(query), null));
		authManager.logout();
		authManager.login("philipp", "password");

		// check the user permissions
		assertTrue(authManager.hasPermission(Permission.get.create()));
		assertTrue(authManager.hasPermission(Permission.load.create()));
		assertTrue(authManager
				.hasPermission(Permission.query.create("myModel")));

		// check the role permissions, MODEL.myOtherModel.query was revoked
		assertTrue(authManager.hasPermission(Permission.connectTSQL.create()));
		assertFalse(authManager.hasPermission(Permission.query
				.create("myOtherModel")));

		authManager.logout();
		authManager.login("admin", "password");

		// revoke some permissions from the user
		query = "REVOKE PERMISSIONS 'GLOBAL.get', 'GLOBAL.load' FROM USER 'philipp'";
		assertNotNull((RevokeResult) factory.evaluateQuery(q(query), null));
		authManager.logout();
		authManager.login("philipp", "password");

		// check the user permissions
		assertFalse(authManager.hasPermission(Permission.get.create()));
		assertFalse(authManager.hasPermission(Permission.load.create()));
		assertTrue(authManager
				.hasPermission(Permission.query.create("myModel")));

		// check the role permissions, MODEL.myOtherModel.query was revoked
		assertTrue(authManager.hasPermission(Permission.connectTSQL.create()));
		assertFalse(authManager.hasPermission(Permission.query
				.create("myOtherModel")));

		authManager.logout();
		authManager.login("admin", "password");

		// revoke all role and user permissions
		query = "REVOKE PERMISSIONS 'GLOBAL.connectTSQL' FROM ROLE 'adult'";
		assertNotNull((RevokeResult) factory.evaluateQuery(q(query), null));
		query = "REVOKE PERMISSIONS 'MODEL.myModel.query' FROM USER 'philipp'";
		assertNotNull((RevokeResult) factory.evaluateQuery(q(query), null));
		authManager.logout();
		authManager.login("philipp", "password");

		// check the user permissions
		assertFalse(authManager.hasPermission(Permission.get.create()));
		assertFalse(authManager.hasPermission(Permission.load.create()));
		assertFalse(authManager.hasPermission(Permission.query
				.create("myModel")));

		// check the role permissions, MODEL.myOtherModel.query was revoked
		assertFalse(authManager.hasPermission(Permission.connectTSQL.create()));
		assertFalse(authManager.hasPermission(Permission.query
				.create("myOtherModel")));

		// cleanUp in the end
		authManager.logout();
		authManager.release();
	}
}
