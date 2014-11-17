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
import net.meisen.dissertation.impl.parser.query.grant.GrantResult;
import net.meisen.dissertation.impl.parser.query.grant.GrantType;
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
		@SystemProperty(property = "test.tmpFolder", value = "testGrantQuery") })
public class TestGrantQueries extends LoaderBasedTest {
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
					System.getProperty("java.io.tmpdir"), "testGrantQuery"));
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
	 * Tests the parsing of a {@code GrantQuery} for a user.
	 * 
	 * @throws ParseException
	 *             if the parsing fails
	 */
	@Test
	public void testGrantUserQueryParsing() throws ParseException {
		GrantQuery q;

		q = q("GRANT PERMISSIONS 'GLOBAL.get', 'MODEL.myModel.query', 'GLOBAL.load' TO USER 'philipp'");
		assertEquals(GrantType.USER, q.getEntityType());
		assertEquals("philipp", q.getEntityName());
		assertEquals(3, q.getPermissions().size());
		assertTrue(q.getPermissions().contains(Permission.get.create()));
		assertTrue(q.getPermissions().contains(Permission.load.create()));
		assertTrue(q.getPermissions().contains(
				Permission.query.create("myModel")));

		q = q("GRANT PERMISSIONS 'GLOBAL.get' TO USER 'philipp'");
		assertEquals(GrantType.USER, q.getEntityType());
		assertEquals("philipp", q.getEntityName());
		assertEquals(1, q.getPermissions().size());
		assertTrue(q.getPermissions().contains(Permission.get.create()));
	}

	/**
	 * Tests the parsing of a {@code GrantQuery} for roles.
	 * 
	 * @throws ParseException
	 *             if the parsing fails
	 */
	@Test
	public void testGrantRoleQueryParsing() throws ParseException {
		GrantQuery q;

		q = q("GRANT PERMISSIONS 'GLOBAL.get', 'MODEL.myModel.query', 'GLOBAL.load' TO ROLE 'sampleRole'");
		assertEquals(GrantType.ROLE, q.getEntityType());
		assertEquals("sampleRole", q.getEntityName());
		assertEquals(3, q.getPermissions().size());
		assertTrue(q.getPermissions().contains(Permission.get.create()));
		assertTrue(q.getPermissions().contains(Permission.load.create()));
		assertTrue(q.getPermissions().contains(
				Permission.query.create("myModel")));

		q = q("GRANT PERMISSIONS 'GLOBAL.get' TO ROLE 'sampleRole'");
		assertEquals(GrantType.ROLE, q.getEntityType());
		assertEquals("sampleRole", q.getEntityName());
		assertEquals(1, q.getPermissions().size());
		assertTrue(q.getPermissions().contains(Permission.get.create()));
	}

	/**
	 * Tests the evaluation of a {@code GrantQuery}.
	 */
	@Test
	public void testGrantQueryEvaluation() {
		String query;

		// add some things we need
		authManager.login("admin", "password");
		factory.evaluateQuery(q("ADD USER 'philipp' WITH PASSWORD 'password'"),
				null);
		factory.evaluateQuery(q("ADD ROLE 'adult'"), null);
		authManager.assignRoleToUser("philipp", "adult");

		// check pre-requirements
		authManager.logout();
		authManager.login("philipp", "password");
		assertFalse(authManager.hasPermission(Permission.get.create()));
		assertFalse(authManager.hasPermission(Permission.load.create()));
		assertFalse(authManager.hasPermission(Permission.query
				.create("myModel")));
		authManager.logout();
		authManager.login("admin", "password");

		// grant some permissions to the user
		query = "GRANT PERMISSIONS 'GLOBAL.get', 'MODEL.myModel.query', 'GLOBAL.load' TO USER 'philipp'";
		assertNotNull((GrantResult) factory.evaluateQuery(q(query), null));
		authManager.logout();
		authManager.login("philipp", "password");
		assertTrue(authManager.hasPermission(Permission.get.create()));
		assertTrue(authManager.hasPermission(Permission.load.create()));
		assertTrue(authManager
				.hasPermission(Permission.query.create("myModel")));
		assertFalse(authManager.hasPermission(Permission.connectTSQL.create()));
		assertFalse(authManager.hasPermission(Permission.query
				.create("myOtherModel")));
		authManager.logout();
		authManager.login("admin", "password");

		// grant some permissions to the role
		query = "GRANT PERMISSIONS 'GLOBAL.connectTSQL', 'MODEL.myOtherModel.query' TO ROLE 'adult'";
		assertNotNull((GrantResult) factory.evaluateQuery(q(query), null));
		authManager.logout();
		authManager.login("philipp", "password");
		assertTrue(authManager.hasPermission(Permission.get.create()));
		assertTrue(authManager.hasPermission(Permission.load.create()));
		assertTrue(authManager
				.hasPermission(Permission.query.create("myModel")));
		assertTrue(authManager.hasPermission(Permission.connectTSQL.create()));
		assertTrue(authManager.hasPermission(Permission.query
				.create("myOtherModel")));

		// cleanUp in the end
		authManager.logout();
		authManager.release();
	}
}
