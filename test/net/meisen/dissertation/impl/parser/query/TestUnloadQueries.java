package net.meisen.dissertation.impl.parser.query;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.impl.parser.query.unload.UnloadQuery;
import net.meisen.dissertation.impl.parser.query.unload.UnloadResult;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the parsing and evaluation of an {@code UnloadQuery}.
 * 
 * @author pmeisen
 * 
 */
public class TestUnloadQueries extends LoaderBasedTest {

	@Autowired
	@Qualifier(DefaultValues.QUERYFACTORY_ID)
	private QueryFactory factory;

	/**
	 * Tests the parsing of a {@code LoadQuery}.
	 */
	@Test
	public void testParsing() {
		UnloadQuery query;

		// parse the load query of an existing model
		query = factory.parseQuery("unLOAD myModel");
		assertEquals("myModel", query.getModelId());

		// parse the load query with a path
		query = factory.parseQuery("UNload myModel");
		assertEquals("myModel", query.getModelId());
	}

	/**
	 * Tests the reloading of a {@code TidaModel}.
	 */
	@Test
	public void testEvaluation() {

		// load the model and unload it again
		m("/net/meisen/dissertation/impl/parser/query/testPersonModel.xml");

		// now load it via the query
		final UnloadQuery query = factory.parseQuery("UNLOAD testPersonModel");
		assertNotNull(loader.getTidaModel("testPersonModel"));
		final UnloadResult res = factory.evaluateQuery(query, null);
		assertNull(loader.getTidaModel("testPersonModel"));
		assertNotNull(res);
	}
}
