package net.meisen.dissertation.impl.parser.query;

import static org.junit.Assert.assertEquals;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.impl.parser.query.insert.InsertQuery;
import net.meisen.dissertation.model.parser.query.IQuery;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the implementation of the {@code QueryFactory} and
 * {@code QueryGenerator}.
 * 
 * @author pmeisen
 * 
 * @see QueryGenerator
 * @see QueryFactory
 * 
 */
public class TestInsertQueries extends LoaderBasedTest {

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
	 * Tests the query which selects all timeseries.
	 */
	@Test
	public void testInsertQuerySingleRecord() {
		final InsertQuery query = q("INSERT INTO MyModel ([START], [END], NAME, PRIORITY) VALUES (20.01.1981, 20.02.2004, 'Philipp', '1')");

		assertEquals("MyModel", query.getModelId());
	}

	@Test
	public void testInsertQueryMultipleRecords() {
		final InsertQuery query = q("INSERT INTO MyModel ([START], [END], NAME, PRIORITY) VALUES (20.01.1981, NULL, 'Philipp', '1'), (NULL, 20.01.1981, 'Nobody', '1')");

		assertEquals("MyModel", query.getModelId());
	}
}
