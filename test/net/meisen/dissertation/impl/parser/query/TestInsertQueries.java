package net.meisen.dissertation.impl.parser.query;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.text.ParseException;
import java.util.List;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.QueryParsingException;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.impl.parser.query.insert.InsertQuery;
import net.meisen.dissertation.model.data.DataStructure;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry;
import net.meisen.dissertation.model.datastructure.MetaStructureEntry;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.general.genmisc.types.Dates;

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
	 * 
	 * @throws ParseException
	 */
	@Test
	public void testInsertQuerySingleRecord() throws ParseException {
		final InsertQuery query = q("INSERT INTO MyModel ([START], [END-], NAME, PRIORITY) VALUES (20.01.1981, 20.02.2004, 'Philipp', '1')");

		assertEquals("MyModel", query.getModelId());
		assertEquals(IntervalType.INCLUDE, query.getStartIntervalType());
		assertEquals(IntervalType.EXCLUDE, query.getEndIntervalType());
		assertEquals(0, query.getStartPosition());
		assertEquals(1, query.getEndPosition());
		assertEquals(1, query.sizeOfRecords());

		assertEquals(IntervalType.INCLUDE, query.getInterval(0).getOpenType());
		assertEquals(IntervalType.EXCLUDE, query.getInterval(0).getCloseType());
		assertEquals(Dates.parseDate("20.01.1981", "dd.MM.yyyy"), query
				.getInterval(0).getStart());
		assertEquals(Dates.parseDate("20.02.2004", "dd.MM.yyyy"), query
				.getInterval(0).getEnd());

		assertArrayEquals(new Object[] { "Philipp", "1" }, query
				.getDescriptorValues(0).toArray());
	}

	/**
	 * Tests the parsing of an insert-statement with multiple records.
	 * 
	 * @throws ParseException
	 *             if a date cannot be parsed
	 */
	@Test
	public void testInsertQueryMultipleRecords() throws ParseException {
		final InsertQuery query = q("INSERT INTO MyModel ([END], NAME, [START+], PRIORITY) VALUES (20.01.1981, 'Philipp', NULL, '2'), (NULL, 'Nobody', 20.01.1981, '1')");

		assertEquals("MyModel", query.getModelId());
		assertEquals(IntervalType.INCLUDE, query.getStartIntervalType());
		assertEquals(IntervalType.INCLUDE, query.getEndIntervalType());
		assertEquals(2, query.getStartPosition());
		assertEquals(0, query.getEndPosition());
		assertEquals(2, query.sizeOfRecords());
		assertEquals(2, query.sizeOfRecords());

		assertEquals(IntervalType.INCLUDE, query.getInterval(0).getOpenType());
		assertEquals(IntervalType.INCLUDE, query.getInterval(0).getCloseType());
		assertNull(query.getInterval(0).getStart());
		assertEquals(Dates.parseDate("20.01.1981", "dd.MM.yyyy"), query
				.getInterval(0).getEnd());

		assertArrayEquals(new Object[] { "Philipp", "2" }, query
				.getDescriptorValues(0).toArray());

		assertEquals(IntervalType.INCLUDE, query.getInterval(1).getOpenType());
		assertEquals(IntervalType.INCLUDE, query.getInterval(1).getCloseType());
		assertEquals(Dates.parseDate("20.01.1981", "dd.MM.yyyy"), query
				.getInterval(1).getStart());
		assertNull(query.getInterval(1).getEnd());

		assertArrayEquals(new Object[] { "Nobody", "1" }, query
				.getDescriptorValues(1).toArray());
	}

	/**
	 * Tests the exception to be thrown if several position parsers are used.
	 */
	@Test
	public void testInvalidUsageOfSeveralPositionMarkers() {
		thrown.expect(QueryParsingException.class);
		thrown.expectMessage("several start- and/or end-positions defined");

		// parsing should fail
		q("INSERT INTO MyModel ([START], [END], NAME, [START+]) VALUES (20.01.1981, NULL, 'Philipp', 20.01.1981)");
	}

	/**
	 * Tests the definition to be thrown if the amount of values does not fit
	 * the amount of defined models.
	 */
	@Test
	public void testInvalidDefinitionOfValues() {
		thrown.expect(QueryParsingException.class);
		thrown.expectMessage("defined values of the descriptors '[Philipp, Tobias]' do not fit with the defined models '[NAME]'");

		// parsing should fail
		q("INSERT INTO MyModel ([START], [END], NAME) VALUES (20.01.1981, NULL, 'Philipp', 'Tobias')");
	}

	/**
	 * Test the implementation of a {@code InsertQuery#getDataStructure()}.
	 */
	@Test
	public void testGetDataStructure() {
		final InsertQuery query = q("INSERT INTO MyModel ([END-], NAME, [START+], PRIORITY, TASK, CONTRACT) VALUES (NULL, 'Philipp', NULL, '2', 'Cleaning', 'Permanent')");

		final DataStructure ds = query.getDataStructure();
		assertEquals(6, ds.getSize());

		// check the IntervalStructureEntries
		final List<IntervalStructureEntry> ise = ds
				.getEntriesByClass(IntervalStructureEntry.class);
		assertEquals(2, ise.size());
		assertEquals(1, ise.get(0).getPosition());
		assertTrue(ise.get(0).isInclusive());
		assertEquals(2, ise.get(1).getPosition());
		assertFalse(ise.get(1).isInclusive());

		// check the MetaStructureEntries
		final List<MetaStructureEntry> mse = ds
				.getEntriesByClass(MetaStructureEntry.class);
		assertEquals(4, mse.size());
		assertEquals(3, mse.get(0).getPosition());
		assertEquals("NAME", mse.get(0).getDescriptorModel());
		assertEquals(4, mse.get(1).getPosition());
		assertEquals("PRIORITY", mse.get(1).getDescriptorModel());
		assertEquals(5, mse.get(2).getPosition());
		assertEquals("TASK", mse.get(2).getDescriptorModel());
		assertEquals(6, mse.get(3).getPosition());
		assertEquals("CONTRACT", mse.get(3).getDescriptorModel());
	}

	@Test
	public void testLala() {

		// load the Model
		final String xml = "/net/meisen/dissertation/impl/parser/query/testEmptyModel.xml";
		final TidaModel model = m(xml);
		assertEquals(0, model.getAmountOfRecords());
		assertEquals(0, model.getNextDataId());

		// evaluate the query
		final InsertQuery query = q("INSERT INTO testEmptyModel ([END], NAME, [START+], PRIORITY, POSITION) VALUES (20.01.2000, 'Philipp', NULL, 'High', '19')");
		factory.evaluateQuery(query);

		// check the result
		model.bulkLoadData(query.getDataStructure(), query.it());
		
		// check if the data was added
		assertEquals(1, model.getAmountOfRecords());
		assertEquals(1, model.getNextDataId());
	}
}
