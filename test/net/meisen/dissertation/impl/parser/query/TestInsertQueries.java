package net.meisen.dissertation.impl.parser.query;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.text.ParseException;
import java.util.List;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.QueryParsingException;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.impl.descriptors.GeneralDescriptor;
import net.meisen.dissertation.impl.descriptors.IntegerDescriptor;
import net.meisen.dissertation.impl.parser.query.insert.InsertQuery;
import net.meisen.dissertation.impl.parser.query.insert.InsertResult;
import net.meisen.dissertation.model.data.DataStructure;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry;
import net.meisen.dissertation.model.datastructure.MetaStructureEntry;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.indexes.datarecord.slices.SliceWithDescriptors;
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

	/**
	 * Tests the insertion of one value.
	 */
	@Test
	public void testInsertionOfOne() {

		// load the Model
		final String xml = "/net/meisen/dissertation/impl/parser/query/testEmptyModel.xml";
		final TidaModel model = m(xml);
		assertEquals(0, model.getAmountOfRecords());
		assertEquals(0, model.getNextDataId());

		// evaluate the query
		final InsertQuery query = q("INSERT INTO testEmptyModel ([END], NAME, [START+], PRIORITY, POSITION) VALUES (20.01.2000, 'Philipp', NULL, 'High', '19')");
		final InsertResult res = factory.evaluateQuery(query);

		// check the result
		assertEquals(1, res.getAmount());

		// check if the data was added
		assertEquals(1, model.getAmountOfRecords());
		assertEquals(1, model.getNextDataId());
	}

	/**
	 * Tests the insertion of multiple values.
	 * 
	 * @throws ParseException
	 *             if a date cannot be parsed
	 */
	@Test
	public void testInsertionOfMultiple() throws ParseException {

		// load the Model
		final String xml = "/net/meisen/dissertation/impl/parser/query/testEmptyModel.xml";
		final TidaModel model = m(xml);
		assertEquals(0, model.getAmountOfRecords());
		assertEquals(0, model.getNextDataId());

		// the query
		String q = "INSERT INTO testEmptyModel ";
		q += "([END], NAME, [START+], PRIORITY, POSITION) VALUES ";
		q += "(20.01.2000 23:59:00, 'Philipp', 20.01.2000, 'High', '19'), ";
		q += "(20.01.2000 23:59:00, 'Tobias', 20.01.2000, 'High', '19'), ";
		q += "(20.01.2000 23:59:00, 'Andrea', 20.01.2000, 'High', '22'), ";
		q += "(22.08.2013 23:59:00, 'Edison', 22.08.2013, 'High', '0'), ";
		q += "(05.04.2014, 'Philipp', NULL, 'Normal', '-1'), ";
		q += "(23.04.2014 22:22:22, 'Philipp', 23.04.2014 10:00:00, 'Low', '8')";

		// evaluate the query
		final InsertQuery query = q(q);
		final InsertResult res = factory.evaluateQuery(query);

		// check the result
		assertEquals(6, res.getAmount());

		// check if the data was added
		assertEquals(6, model.getAmountOfRecords());
		assertEquals(6, model.getNextDataId());

		// check the Model
		final MetaDataModel metaDataModel = model.getMetaDataModel();
		DescriptorModel<?> descModel;
		descModel = metaDataModel.getDescriptorModel("NAME");
		assertEquals(Object.class, descModel.getValueType());
		assertEquals(GeneralDescriptor.class, descModel.getDescriptorClass());
		assertEquals(4, descModel.getDescriptors().size());
		assertNotNull(descModel.getDescriptorByValue("Philipp"));
		assertNotNull(descModel.getDescriptorByValue("Tobias"));
		assertNotNull(descModel.getDescriptorByValue("Andrea"));
		assertNotNull(descModel.getDescriptorByValue("Edison"));

		descModel = metaDataModel.getDescriptorModel("PRIORITY");
		assertEquals(Object.class, descModel.getValueType());
		assertEquals(GeneralDescriptor.class, descModel.getDescriptorClass());
		assertEquals(3, descModel.getDescriptors().size());
		assertNotNull(descModel.getDescriptorByValue("High"));
		assertNotNull(descModel.getDescriptorByValue("Normal"));
		assertNotNull(descModel.getDescriptorByValue("Low"));

		descModel = metaDataModel.getDescriptorModel("POSITION");
		assertEquals(Integer.class, descModel.getValueType());
		assertEquals(IntegerDescriptor.class, descModel.getDescriptorClass());
		assertEquals(5, descModel.getDescriptors().size());
		assertNotNull(descModel.getDescriptorByValue(19));
		assertNotNull(descModel.getDescriptorByValue(0));
		assertNotNull(descModel.getDescriptorByValue(8));
		assertNotNull(descModel.getDescriptorByValue(22));
		assertNotNull(descModel.getDescriptorByValue(-1));

		// get slices
		final SliceWithDescriptors<?>[] slices = model.getIndex()
				.getIntervalIndexSlices(
						Dates.parseDate("20.01.2000", "dd.MM.yyyy"),
						Dates.parseDate("21.01.2000", "dd.MM.yyyy"), true,
						false);
		assertEquals(1440, slices.length);

		for (final SliceWithDescriptors<?> s : slices) {
			assertNotNull(s);
			assertEquals(3, s.getDescriptors("NAME").size());
			assertEquals(1, s.getDescriptors("PRIORITY").size());
			assertEquals(2, s.getDescriptors("POSITION").size());
		}
	}
}
