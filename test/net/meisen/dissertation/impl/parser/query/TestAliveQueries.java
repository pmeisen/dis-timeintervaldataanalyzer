package net.meisen.dissertation.impl.parser.query;

import static org.junit.Assert.assertNotNull;
import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.impl.parser.query.alive.AliveQuery;
import net.meisen.dissertation.impl.parser.query.alive.AliveResult;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the parsing and evaluation of an {@code AliveQuery}.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
@RunWith(JUnitConfigurationRunner.class)
public class TestAliveQueries {

	@Autowired
	@Qualifier(DefaultValues.QUERYFACTORY_ID)
	private QueryFactory factory;

	/**
	 * Tests the parsing of an {@code AliveQuery}.
	 */
	@Test
	public void testParsing() {
		assertNotNull(factory.parseQuery("ALIVE"));
		assertNotNull(factory.parseQuery("alive"));
		assertNotNull(factory.parseQuery("aLive"));
	}

	/**
	 * Tests the evaluation of an {@code AliveQuery}.
	 */
	@Test
	public void testEvaluation() {
		final AliveQuery query = factory.parseQuery("ALIVE");
		final AliveResult result = factory.evaluateQuery(query, null);

		assertNotNull(result);
	}
}
