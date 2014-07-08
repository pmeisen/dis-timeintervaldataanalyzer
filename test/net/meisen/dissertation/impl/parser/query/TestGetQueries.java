package net.meisen.dissertation.impl.parser.query;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.text.ParseException;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.impl.parser.query.get.GetQuery;
import net.meisen.dissertation.impl.parser.query.get.GetResult;
import net.meisen.dissertation.impl.parser.query.get.GetResultType;
import net.meisen.dissertation.jdbc.protocol.QueryType;
import net.meisen.dissertation.model.parser.query.IQuery;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the implementation of {@link GetQuery} and {@link GetResult}.
 * 
 * @author pmeisen
 * 
 */
public class TestGetQueries extends LoaderBasedTest {

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
	 * Tests the parsing.
	 * 
	 * @throws ParseException
	 *             if the parsing fails
	 */
	@Test
	public void testQueryParsing() throws ParseException {
		final GetQuery q = q("GET MODELS");

		assertEquals(QueryType.QUERY, q.getQueryType());
		assertEquals(GetResultType.MODELS, q.getResultType());
		assertNull(q.getModelId());
	}

	/**
	 * Tests the retrieval of no values.
	 */
	@Test
	public void testQueryEmptyRetrieval() {
		final IQuery query = factory.parseQuery("GET MODELS");
		final GetResult res = factory.evaluateQuery(query, null);
		assertEquals(0, res.size());
		assertFalse(res.iterator().hasNext());
	}

	/**
	 * Tests the retrieval of values.
	 */
	@Test
	public void testQueryRetrieval() {

		// load the testModel
		m("/net/meisen/dissertation/impl/parser/query/testEmptyNumberModel.xml");
		m("/net/meisen/dissertation/impl/parser/query/testPersonModel.xml");

		final IQuery query = factory.parseQuery("GET MODELS");
		final GetResult res = factory.evaluateQuery(query, null);
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
}
