package net.meisen.dissertation.impl.parser.query;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.File;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.AuthException;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.impl.auth.shiro.ShiroAuthManager;
import net.meisen.dissertation.impl.parser.query.modify.ModifyQuery;
import net.meisen.dissertation.impl.parser.query.modify.ModifyResult;
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
 * Tests the implementation of the {@code ModifyQuery} statement.
 * 
 * @author pmeisen
 * 
 */
@SystemProperties(value = {
		@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/impl/parser/query/tidaConfigUsingShiroAuth.xml"),
		@SystemProperty(property = "test.tmpFolder", value = "testModifyQuery") })
public class TestModifyQueries extends LoaderBasedTest {
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
					System.getProperty("java.io.tmpdir"), "testModifyQuery"));
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
	 * Tests the parsing of a modify-user-statement.
	 */
	@Test
	public void testModifyUserParsing() {
		final ModifyQuery q = q("MODIFY USER 'philipp' SET PASSWORD = 'newpassword'");
		assertNull(q.getModelId());
		assertEquals("philipp", q.getEntityName());
		assertEquals("newpassword", q.getEntityValue());
	}

	/**
	 * Tests the parsing of a modify-model-statement.
	 */
	@Test
	public void testModifyModelParsing() {
		final ModifyQuery q = q("MODIFY MODEL myModel SET BULKLOAD = true");
		assertEquals("myModel", q.getModelId());
		assertEquals("myModel", q.getEntityName());
		assertEquals("true", q.getEntityValue());
	}

	/**
	 * Tests the evaluation of a {@code AddQuery}.
	 */
	@Test
	public void testModifyUserQueryEvaluation() {
		boolean exception;

		String query;

		// add some stuff which can be modified
		authManager.login("admin", "password");
		factory.evaluateQuery(q("ADD USER 'philipp' WITH PASSWORD 'password'"),
				null);

		// modify a user
		authManager.logout();
		authManager.login("philipp", "password");
		authManager.logout();
		authManager.login("admin", "password");
		query = "MODIFY USER 'philipp' SET PASSWORD = 'newpassword'";
		assertNotNull((ModifyResult) factory.evaluateQuery(q(query), null));
		authManager.logout();
		try {
			exception = false;
			authManager.login("philipp", "password");
		} catch (final AuthException e) {
			exception = true;
		}
		assertTrue(exception);
		authManager.login("philipp", "newpassword");

		// cleanUp in the end
		authManager.logout();
	}

	/**
	 * Tests the evaluation of the {@code ModifyQuery} to modify a model's
	 * bulk-load property.
	 */
	@Test
	public void testModifyModelQueryEvaluation() {
		authManager.login("admin", "password");

		// load the Model
		final String xml = "/net/meisen/dissertation/impl/parser/query/testEmptyModel.xml";
		TidaModel model = m(xml);

		assertFalse(model.isBulkload());
		factory.evaluateQuery(
				q("MODIFY MODEL testEmptyModel SET BULKLOAD = true"), null);
		assertTrue(model.isBulkload());
		factory.evaluateQuery(
				q("MODIFY MODEL testEmptyModel SET BULKLOAD = false"), null);
		assertFalse(model.isBulkload());

		// cleanUp in the end
		authManager.logout();
	}
}
