package net.meisen.dissertation.impl.parser.query;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.text.ParseException;
import java.util.Arrays;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.impl.parser.query.delete.DeleteQuery;
import net.meisen.dissertation.impl.parser.query.delete.DeleteResult;
import net.meisen.dissertation.impl.parser.query.insert.InsertQuery;
import net.meisen.dissertation.impl.parser.query.insert.InsertResult;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IQueryResult;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import com.google.common.primitives.Ints;

/**
 * Tests the implementation of the {@code DeleteQuery}.
 * 
 * @author pmeisen
 * 
 */
public class TestDeleteQueries extends LoaderBasedTest {

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
	 * Tests the parsing of the delete queries.
	 */
	@Test
	public void testParsingOfDeleteQueries() {
		DeleteQuery query;

		query = q("DELETE 5 FROM myModelId");
		assertEquals("myModelId", query.getModelId());
		assertEquals(1, query.getIdList().length);
		assertEquals(5, query.getIdList()[0]);

		query = q("DELETE 1, 7, 10, 11 FROM myModelId");
		assertEquals("myModelId", query.getModelId());
		assertEquals(4, query.getIdList().length);
		assertEquals(1, query.getIdList()[0]);
		assertEquals(7, query.getIdList()[1]);
		assertEquals(10, query.getIdList()[2]);
		assertEquals(11, query.getIdList()[3]);
	}

	@Test
	public void testDeleteQuerySingleRecord() throws ParseException {
		DeleteResult res;
		DeleteQuery query;

		// load the Model
		final String xml = "/net/meisen/dissertation/impl/parser/query/testEmptyModel.xml";
		final TidaModel model = m(xml);

		// add a record to the set
		factory.evaluateQuery(
				q("INSERT INTO testEmptyModel ([END], NAME, [START+], PRIORITY, POSITION) VALUES (20.01.2000, 'Philipp', NULL, 'High', '19')"),
				null);
		assertEquals(1, model.getAmountOfRecords());

		// delete the record again
		res = factory.evaluateQuery(q("DELETE " + (model.getNextDataId() - 1)
				+ " FROM testEmptyModel"), null);
		assertEquals(1, res.getAmount());
		assertEquals(0, model.getAmountOfRecords());
		assertEquals(1, model.getNextDataId());

		// delete it again, the amount should have changed
		res = factory.evaluateQuery(q("DELETE " + (model.getNextDataId() - 1)
				+ " FROM testEmptyModel"), null);
		assertEquals(0, res.getAmount());
		assertEquals(0, model.getAmountOfRecords());

		// add several records
		for (int i = 0; i < 10; i++) {
			factory.evaluateQuery(
					q("INSERT INTO testEmptyModel ([END], NAME, [START+], PRIORITY, POSITION) VALUES (20.01.2000, 'Philipp', NULL, 'High', '19')"),
					null);
		}
		assertEquals(10, model.getAmountOfRecords());
		assertEquals(11, model.getNextDataId());

		query = q("DELETE 2, 4, 6, 8, 10 FROM testEmptyModel");
		query.enableIdCollection(true);
		res = factory.evaluateQuery(query, null);
		assertEquals(5, res.getAmount());
		assertTrue(Arrays.binarySearch(res.getCollectedIds(), 2) > -1);
		assertTrue(Arrays.binarySearch(res.getCollectedIds(), 4) > -1);
		assertTrue(Arrays.binarySearch(res.getCollectedIds(), 6) > -1);
		assertTrue(Arrays.binarySearch(res.getCollectedIds(), 8) > -1);
		assertTrue(Arrays.binarySearch(res.getCollectedIds(), 10) > -1);

		query = q("DELETE 1, 2, 3 FROM testEmptyModel");
		query.enableIdCollection(true);
		res = factory.evaluateQuery(query, null);
		assertEquals(2, res.getAmount());
		assertTrue(Arrays.binarySearch(res.getCollectedIds(), 1) > -1);
		assertFalse(Arrays.binarySearch(res.getCollectedIds(), 2) > -1);
		assertTrue(Arrays.binarySearch(res.getCollectedIds(), 3) > -1);
	}
}
