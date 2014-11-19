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
import net.meisen.dissertation.model.data.TidaModel;
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
					System.getProperty("java.io.tmpdir"), "testDropQuery"));
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
	 * Tests the parsing of a drop-model-statement.
	 */
	@Test
	public void testDropModelParsing() {
		final DropQuery q = q("DROP MODEL myModel");
		assertEquals(DropType.MODEL, q.getEntityType());
		assertEquals("myModel", q.getEntityName());
	}

	/**
	 * Tests the evaluation of a {@code AddQuery}.
	 */
	@Test
	public void testDropQueryUserAndRoleEvaluation() {
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
	}

	/**
	 * Tests the dropping of a model.
	 */
	@Test
	public void testDropQueryModelEvaluation() {
		authManager.login("admin", "password");

		// load the Model
		final String xml = "/net/meisen/dissertation/impl/parser/query/testDropModel.xml";
		TidaModel model = m(xml);
		assertEquals(0, model.getAmountOfRecords());
		assertEquals(0, model.getNextDataId());

		// add some data
		for (int i = 0; i < 100; i++) {
			factory.evaluateQuery(
					q("INSERT INTO testDropModel ([START], [END], VALUE) VALUES (01.01.2008 00:00:00, 01.01.2008 10:00:00, 'Philipp')"),
					null);
		}
		assertEquals(100, model.getAmountOfRecords());
		assertEquals(100, model.getNextDataId());

		// drop and load again
		factory.evaluateQuery(q("DROP MODEL testDropModel"), null);
		model = m(xml);
		assertEquals(0, model.getAmountOfRecords());
		assertEquals(0, model.getNextDataId());
		factory.evaluateQuery(q("DROP MODEL testDropModel"), null);

		authManager.logout();

		// remove the folders for sure
		Files.deleteOnExitDir(new File(System.getProperty("java.io.tmpdir"),
				"testDropModelIndex"));
		Files.deleteOnExitDir(new File(System.getProperty("java.io.tmpdir"),
				"testDropModel"));
	}
}
