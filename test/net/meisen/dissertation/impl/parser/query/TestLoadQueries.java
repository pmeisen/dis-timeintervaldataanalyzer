package net.meisen.dissertation.impl.parser.query;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.InputStream;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.impl.parser.query.load.LoadQuery;
import net.meisen.dissertation.impl.parser.query.load.LoadResult;
import net.meisen.dissertation.model.parser.query.IResourceResolver;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the parsing and evaluation of an {@code LoadQuery}.
 * 
 * @author pmeisen
 * 
 */
public class TestLoadQueries extends LoaderBasedTest implements
		IResourceResolver {

	@Autowired
	@Qualifier(DefaultValues.QUERYFACTORY_ID)
	private QueryFactory factory;

	/**
	 * Tests the parsing of a {@code LoadQuery}.
	 */
	@Test
	public void testParsing() {
		LoadQuery query;

		// parse the load query of an existing model
		query = factory.parseQuery("LOAD myModel");
		assertNull(query.getPath());
		assertEquals("myModel", query.getModelId());
		assertNull(query.getProperty("autoload", null));

		// parse the load query with a path
		query = factory.parseQuery("load FROM 'C:\\\\'");
		assertNull(query.getModelId());
		assertEquals("C:\\", query.getPath());
		assertNull(query.getProperty("autoload", null));

		// parse the load query with properties
		query = factory.parseQuery("load FROM 'C:\\\\' SET autoload=true");
		assertNull(query.getModelId());
		assertEquals("C:\\", query.getPath());
		assertEquals(true, query.getProperty("autoload", null));

		// parse the load query with properties
		query = factory
				.parseQuery("load FROM 'C:\\\\' SET autoload=true, force=true");
		assertNull(query.getModelId());
		assertEquals("C:\\", query.getPath());
		assertEquals(true, query.getProperty("autoload", null));
		assertEquals(true, query.getProperty("force", null));
	}

	/**
	 * Tests the reloading of a {@code TidaModel}.
	 */
	@Test
	public void testEvaluatingWithPreviouslyLoadedModel() {

		// load the model and unload it again
		m("/net/meisen/dissertation/impl/parser/query/testPersonModel.xml");
		loader.unload("testPersonModel");

		// now load it via the query
		final LoadQuery query = factory.parseQuery("LOAD testPersonModel");
		assertNull(loader.getTidaModel("testPersonModel"));
		final LoadResult res = factory.evaluateQuery(query, null);
		assertNotNull(loader.getTidaModel("testPersonModel"));
		assertNotNull(res);
		assertEquals("testPersonModel", res.getModelId());
	}

	/**
	 * Loads a {@code TidaModel} from a specified location using {@code this} as
	 * {@code ResourceResolver}.
	 */
	@Test
	public void testEvaluatingWithToBeLoadedModel() {

		// load the data from the classpath
		final LoadQuery query = factory
				.parseQuery("LOAD FROM '/net/meisen/dissertation/impl/parser/query/testPersonModel.xml'");

		assertNull(loader.getTidaModel("testPersonModel"));
		final LoadResult res = factory.evaluateQuery(query, this);
		assertNotNull(loader.getTidaModel("testPersonModel"));
		assertNotNull(res);
		assertEquals("testPersonModel", res.getModelId());
	}

	/**
	 * Tests the loading of an already loaded {@code TidaModel}.
	 */
	@Test
	public void testExceptionWhenEvaluatingAlreadyLoadedModel() {
		thrown.expect(QueryEvaluationException.class);
		thrown.expectMessage("model 'testPersonModel' is already loaded");

		// load the model and unload it again
		m("/net/meisen/dissertation/impl/parser/query/testPersonModel.xml");

		// now load it via the query
		final LoadQuery query = factory.parseQuery("LOAD testPersonModel set force=true");
		factory.evaluateQuery(query, null);
	}

	/**
	 * Tests the loading of a resource without a {@code ResourceResolver}.
	 */
	@Test
	public void testExceptionWhenEvaluatingWithoutNeededResolver() {
		thrown.expect(QueryEvaluationException.class);
		thrown.expectMessage("resolve the path 'C:\\' without resolver");

		// now load it via the query
		final LoadQuery query = factory.parseQuery("LOAD FROM 'C:\\\\'");
		factory.evaluateQuery(query, null);
	}

	/**
	 * Tests the loading of an invalid (i.e. not available) {@code TidaModel}.
	 */
	@Test
	public void testExceptionWhenEvaluatingWithInvalidModel() {
		thrown.expect(QueryEvaluationException.class);
		thrown.expectMessage("system cannot load the model 'myModel'");

		final LoadQuery query = factory.parseQuery("LOAD myModel");
		factory.evaluateQuery(query, null);
	}

	@Override
	public InputStream resolve(final String resource) {
		return getClass().getResourceAsStream(resource);
	}
}
