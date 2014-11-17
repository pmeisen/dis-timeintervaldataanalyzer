package net.meisen.dissertation.impl.parser.query;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.text.ParseException;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.impl.auth.shiro.ShiroAuthManager;
import net.meisen.dissertation.impl.parser.query.get.GetQuery;
import net.meisen.dissertation.impl.parser.query.get.GetResultModels;
import net.meisen.dissertation.impl.parser.query.get.GetResultPermissions;
import net.meisen.dissertation.impl.parser.query.get.GetResultRoles;
import net.meisen.dissertation.impl.parser.query.get.GetResultType;
import net.meisen.dissertation.impl.parser.query.get.GetResultUsers;
import net.meisen.dissertation.impl.parser.query.get.GetResultVersion;
import net.meisen.dissertation.jdbc.protocol.QueryType;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperties;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the implementation of {@link GetQuery} and {@link GetResultModels}.
 * 
 * @author pmeisen
 * 
 */
@SystemProperties(value = {
		@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/impl/parser/query/tidaConfigUsingShiroAuth.xml"),
		@SystemProperty(property = "test.tmpFolder", value = "testGetQueries") })
public class TestGetQueries extends LoaderBasedTest {
	private static TidaModelHandler h = null;

	@Autowired
	@Qualifier(DefaultValues.MODELHANDLER_ID)
	private TidaModelHandler handler;

	@Autowired
	@Qualifier(DefaultValues.AUTHMANAGER_ID)
	private ShiroAuthManager authManager;

	@Autowired
	@Qualifier(DefaultValues.QUERYFACTORY_ID)
	private QueryFactory factory;

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
					System.getProperty("java.io.tmpdir"), "testGetQueries"));
		} else {
			h.unloadAll();
			Files.deleteOnExitDir(new File(h.getDefaultLocation()));
		}
	}

	/**
	 * Initializes the tests by setting the static handler {@code h}.
	 */
	@Before
	public void setAndLogin() {
		authManager.login("admin", "password");

		if (h == null) {
			h = handler;
		} else {
			assertTrue(h == handler);
		}
	}

	/**
	 * Make sure that the user is logged out.
	 */
	@After
	public void logout() {
		authManager.logout();
	}

	/**
	 * Tests the parsing.
	 * 
	 * @throws ParseException
	 *             if the parsing fails
	 */
	@Test
	public void testQueryParsing() throws ParseException {
		GetQuery q;

		q = q("GET MODELS");
		assertEquals(QueryType.QUERY, q.getQueryType());
		assertEquals(GetResultType.MODELS, q.getResultType());
		assertNull(q.getModelId());

		q = q("GET VERSION");
		assertEquals(QueryType.QUERY, q.getQueryType());
		assertEquals(GetResultType.VERSION, q.getResultType());
		assertNull(q.getModelId());

		q = q("GET USERS");
		assertEquals(QueryType.QUERY, q.getQueryType());
		assertEquals(GetResultType.USERS, q.getResultType());
		assertNull(q.getModelId());

		q = q("GET ROLES");
		assertEquals(QueryType.QUERY, q.getQueryType());
		assertEquals(GetResultType.ROLES, q.getResultType());
		assertNull(q.getModelId());
	}

	/**
	 * Tests the retrieval of no values.
	 */
	@Test
	public void testQueryModelsEmptyRetrieval() {
		final GetQuery query = q("GET MODELS");
		final GetResultModels res = factory.evaluateQuery(query, null);
		assertEquals(0, res.size());
		assertFalse(res.iterator().hasNext());
	}

	/**
	 * Tests the retrieval of values using a models query.
	 */
	@Test
	public void testQueryModelsRetrieval() {

		// load the testModel
		m("/net/meisen/dissertation/impl/parser/query/testEmptyNumberModel.xml");
		m("/net/meisen/dissertation/impl/parser/query/testPersonModel.xml");

		final GetQuery query = q("GET MODELS");
		final GetResultModels res = factory.evaluateQuery(query, null);
		assertEquals(2, res.size());

		final Set<String> expected = new HashSet<String>();
		expected.add("testNumberModel");
		expected.add("testPersonModel");

		// test the iterator
		final Iterator<Object[]> it = res.iterator();
		Object[] row;
		assertTrue(it.hasNext());
		row = it.next();
		assertTrue(row[0].toString(), expected.remove(row[0]));
		assertTrue(it.hasNext());
		row = it.next();
		assertTrue(row[0].toString(), expected.remove(row[0]));
		assertFalse(it.hasNext());
		assertEquals(0, expected.size());
	}

	/**
	 * Tests the retrieval of values using a version query.
	 */
	@Test
	public void testQueryVersionRetrieval() {

		final GetQuery query = q("GET VERSION");
		final GetResultVersion res = factory.evaluateQuery(query, null);

		// test the iterator
		final Iterator<Object[]> it = res.iterator();
		if (res.hasManifest()) {
			assertTrue(it.hasNext());

			final Object[] row = it.next();
			assertEquals("dis-timeintervaldataanalyzer", row[0]);
			assertNotNull(row[1]);
			assertNotNull(row[2]);
			assertFalse(it.hasNext());
		} else {
			assertFalse(it.hasNext());
		}
	}

	/**
	 * Tests the implementation of the {@code GET ROLES} query.
	 */
	@Test
	public void testQueryRolesRetrieval() {
		Object[] row;
		Iterator<Object[]> it;
		GetResultRoles res;

		// test the "empty" result
		res = factory.evaluateQuery(q("GET ROLES"), null);
		it = res.iterator();
		assertFalse(it.hasNext());

		// add a role
		factory.evaluateQuery(
				q("ADD ROLE 'role' WITH PERMISSIONS 'GLOBAL.get','MODEL.anotherModel.query'"),
				null);

		res = factory.evaluateQuery(q("GET ROLES"), null);
		it = res.iterator();
		assertTrue(it.hasNext());
		row = it.next();
		assertEquals("role", row[0]);
		assertEquals("GLOBAL.get,MODEL.anotherModel.query", row[1]);
		assertFalse(it.hasNext());

		// remove the roles
		factory.evaluateQuery(q("DROP ROLE 'role'"), null);

		// check
		res = factory.evaluateQuery(q("GET ROLES"), null);
		it = res.iterator();
		assertFalse(it.hasNext());
	}

	/**
	 * Tests the retrieval of values using a users query.
	 */
	@Test
	public void testQueryUsersRetrieval() {
		Object[] row;
		Iterator<Object[]> it;
		GetResultUsers res;

		// test the "empty" result
		res = factory.evaluateQuery(q("GET USERS"), null);
		it = res.iterator();
		assertTrue(it.hasNext());
		row = it.next();
		assertEquals("admin", row[0]);
		assertEquals("", row[1]);
		assertTrue(row[2].toString().contains("GLOBAL.load"));
		assertTrue(row[2].toString().contains("MODEL.*.query"));

		// add some user
		factory.evaluateQuery(
				q("ADD USER 'philipp' WITH PASSWORD 'password' WITH PERMISSIONS 'GLOBAL.get','MODEL.myModel.query' WITH ROLES 'user', 'father'"),
				null);

		// check
		res = factory.evaluateQuery(q("GET USERS"), null);
		it = res.iterator();
		assertTrue(it.hasNext());
		row = it.next();
		assertEquals("admin", row[0]);
		assertTrue(it.hasNext());
		row = it.next();
		assertEquals("philipp", row[0]);
		assertEquals("father,user", row[1]);
		assertEquals("GLOBAL.get,MODEL.myModel.query", row[2]);

		// add a role
		factory.evaluateQuery(
				q("ADD ROLE 'user' WITH PERMISSIONS 'GLOBAL.get','MODEL.anotherModel.query'"),
				null);

		// check
		res = factory.evaluateQuery(q("GET USERS"), null);
		it = res.iterator();
		assertTrue(it.hasNext());
		row = it.next();
		assertEquals("admin", row[0]);
		assertTrue(it.hasNext());
		row = it.next();
		assertEquals("philipp", row[0]);
		assertEquals("father,user", row[1]);
		assertEquals("GLOBAL.get,MODEL.anotherModel.query,MODEL.myModel.query",
				row[2]);

		// remove the roles
		factory.evaluateQuery(q("DROP ROLE 'user'"), null);

		// check
		res = factory.evaluateQuery(q("GET USERS"), null);
		it = res.iterator();
		assertTrue(it.hasNext());
		row = it.next();
		assertEquals("admin", row[0]);
		assertTrue(it.hasNext());
		row = it.next();
		assertEquals("philipp", row[0]);
		assertEquals("father,user", row[1]);
		assertEquals("GLOBAL.get,MODEL.myModel.query", row[2]);

		// remove the user to obtain order
		factory.evaluateQuery(q("DROP USER 'philipp'"), null);
	}

	/**
	 * Tests the retrieval of values using a users query.
	 */
	@Test
	public void testQueryPermissionsRetrieval() {
		Object[] row;
		Iterator<Object[]> it;
		GetResultPermissions res;
		int counter;

		// test the "empty" result
		res = factory.evaluateQuery(q("GET PERMISSIONS"), null);
		it = res.iterator();
		while (it.hasNext()) {
			row = it.next();

			final DefinedPermission perm = DefinedPermission
					.fromString((String) row[2]);

			// only admin should be available
			assertEquals("admin", row[0]);

			// differ global and model permissions
			if (row[1] == null) {
				assertTrue(perm.getPermission().isGlobal());
			} else if ("*".equals(row[1])) {
				assertFalse(perm.getPermission().isGlobal());
			} else {
				fail("invalid value");
			}
		}

		// add some user
		factory.evaluateQuery(
				q("ADD USER 'philipp' WITH PASSWORD 'password' WITH PERMISSIONS 'GLOBAL.get','MODEL.myModel.query' WITH ROLES 'user', 'father'"),
				null);

		// check
		res = factory.evaluateQuery(q("GET PERMISSIONS"), null);
		it = res.iterator();
		counter = 0;
		while (it.hasNext()) {
			row = it.next();

			if ("philipp".equals(row[0])) {
				if (row[1] == null) {
					assertEquals("GLOBAL.get", row[2]);
				} else {
					assertEquals("MODEL.myModel.query", row[2]);
				}

				counter++;
			}
		}
		assertEquals(2, counter);

		// add a role
		factory.evaluateQuery(
				q("ADD ROLE 'user' WITH PERMISSIONS 'GLOBAL.get','MODEL.testNumberModel.query'"),
				null);

		// check
		res = factory.evaluateQuery(q("GET PERMISSIONS"), null);
		it = res.iterator();
		counter = 0;
		while (it.hasNext()) {
			row = it.next();

			if ("philipp".equals(row[0])) {
				if (row[1] == null) {
					assertEquals("GLOBAL.get", row[2]);
				} else {
					assertTrue("MODEL.myModel.query".equals(row[2])
							|| "MODEL.testNumberModel.query".equals(row[2]));
				}

				counter++;
			}
		}
		assertEquals(3, counter);

		// load the testModel
		m("/net/meisen/dissertation/impl/parser/query/testEmptyNumberModel.xml");
		m("/net/meisen/dissertation/impl/parser/query/testPersonModel.xml");

		// check
		res = factory.evaluateQuery(q("GET PERMISSIONS"), null);
		it = res.iterator();
		counter = 0;
		while (it.hasNext()) {
			row = it.next();

			if ("admin".equals(row[0])) {
				final DefinedPermission perm = DefinedPermission
						.fromString((String) row[2]);

				// differ global and model permissions
				if (row[1] == null) {
					assertTrue(perm.getPermission().isGlobal());
				} else if ("*".equals(row[1])) {
					assertFalse(perm.getPermission().isGlobal());
				} else if ("testNumberModel".equals(row[1])) {
					assertEquals(perm.getModelId(), "testNumberModel");
					assertFalse(perm.getPermission().isGlobal());
					assertTrue("MODEL.testNumberModel.query".equals(row[2])
							|| "MODEL.testNumberModel.modify".equals(row[2]));
				} else if ("testPersonModel".equals(row[1])) {
					assertEquals(perm.getModelId(), "testPersonModel");
					assertFalse(perm.getPermission().isGlobal());
					assertTrue("MODEL.testPersonModel.query".equals(row[2])
							|| "MODEL.testPersonModel.modify".equals(row[2]));
				} else {
					fail("invalid value: " + row[1]);
				}

			} else if ("philipp".equals(row[0])) {
				if (row[1] == null) {
					assertEquals("GLOBAL.get", row[2]);
				} else {
					assertTrue("MODEL.myModel.query".equals(row[2])
							|| "MODEL.testNumberModel.query".equals(row[2]));
				}

				counter++;
			}
		}
		assertEquals(3, counter);
	}
}
