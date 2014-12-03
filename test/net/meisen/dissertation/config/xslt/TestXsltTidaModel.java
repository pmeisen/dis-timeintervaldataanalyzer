package net.meisen.dissertation.config.xslt;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.ParseException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.config.xslt.mock.MockIndexFactory;
import net.meisen.dissertation.config.xslt.mock.MockMapperFactory;
import net.meisen.dissertation.help.ModuleAndDbBasedTest;
import net.meisen.dissertation.impl.dataretriever.DbDataRetrieverException;
import net.meisen.dissertation.impl.datasets.DataRetrieverDataSet;
import net.meisen.dissertation.impl.datasets.SingleStaticDataSet;
import net.meisen.dissertation.impl.descriptors.DoubleDescriptor;
import net.meisen.dissertation.impl.descriptors.GeneralDescriptor;
import net.meisen.dissertation.impl.descriptors.IntegerDescriptor;
import net.meisen.dissertation.impl.descriptors.ListDescriptor;
import net.meisen.dissertation.impl.descriptors.LongDescriptor;
import net.meisen.dissertation.impl.descriptors.ResourceDescriptor;
import net.meisen.dissertation.impl.indexes.IndexFactory;
import net.meisen.dissertation.impl.time.mapper.MapperFactory;
import net.meisen.dissertation.model.data.DataModel;
import net.meisen.dissertation.model.data.DataStructure;
import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.OfflineMode;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.data.metadata.MetaDataCollection;
import net.meisen.dissertation.model.datasets.IClosableIterator;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry.IntervalTypeFactory.IntervalType;
import net.meisen.dissertation.model.datastructure.MetaStructureEntry;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.time.granularity.DateFormat;
import net.meisen.dissertation.model.time.granularity.Hour;
import net.meisen.dissertation.model.time.granularity.Minute;
import net.meisen.dissertation.model.time.granularity.Second;
import net.meisen.dissertation.model.time.timeline.TimelineDefinition;
import net.meisen.general.genmisc.types.Classes;
import net.meisen.general.genmisc.types.Dates;
import net.meisen.general.sbconfigurator.config.DefaultConfiguration;
import net.meisen.general.sbconfigurator.config.exception.InvalidXsltException;
import net.meisen.general.sbconfigurator.config.exception.TransformationFailedException;
import net.meisen.general.sbconfigurator.config.transformer.DefaultXsltTransformer;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.Before;
import org.junit.Test;
import org.junit.matchers.JUnitMatchers;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;

/**
 * Test to test the transformation of a model using xslt and the interpretation
 * of the created Spring-beans.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TidaConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
public class TestXsltTidaModel extends ModuleAndDbBasedTest {

	/**
	 * the default xslt transformer used for testing
	 */
	private DefaultXsltTransformer transformer;

	/**
	 * {@code OutputStream} to write to
	 */
	private final ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

	/**
	 * Initializes the {@code transformer} to point to the correct {@code xslt}.
	 */
	@Before
	public void init() {
		transformer = (DefaultXsltTransformer) ((DefaultConfiguration) configuration)
				.getXsltTransformer();
		try {
			transformer
					.setXsltTransformer("/net/meisen/dissertation/config/xslt/modelToSpring.xslt");
		} catch (final InvalidXsltException e) {
			e.printStackTrace();
			fail("Unexpected exception '" + e.getMessage() + "'");
		}

	}

	private TidaModel getTidaModel(final String xml) {
		setModulesHolder(xml);
		return modulesHolder.getModule(DefaultValues.TIDAMODEL_ID);
	}

	private MetaDataModel getMetaDataModel(final String xml) {
		setModulesHolder(xml);

		// get the metaData
		final MetaDataCollection collection = modulesHolder
				.getModule(DefaultValues.METADATACOLLECTION_ID);

		// get the metaDataModel
		final MetaDataModel metaDataModel = modulesHolder
				.getModule(DefaultValues.METADATAMODEL_ID);

		// add the metaData
		metaDataModel.addMetaData(collection);

		return metaDataModel;
	}

	private DataModel getDataModel(final String xml) {
		setModulesHolder(xml);
		return modulesHolder.getModule(DefaultValues.DATAMODEL_ID);
	}

	private IntervalModel getIntervalModel(final String xml) {
		setModulesHolder(xml);
		return modulesHolder.getModule(DefaultValues.INTERVALMODEL_ID);
	}

	private DataStructure getDataStructure(final String xml) {
		setModulesHolder(xml);
		return modulesHolder.getModule(DefaultValues.DATASTRUCTURE_ID);
	}

	/**
	 * Tests the replacement of the default {@code IndexFactory}.
	 */
	@Test
	public void testDefaultFactories() {
		final TidaModel model = getTidaModel("/net/meisen/dissertation/config/xslt/configDefaultFactories.xml");
		Class<?> res;

		// check the default IndexFactory
		res = model.getMetaDataModel().getIndexFactory().getClass();
		assertTrue("Instance of '" + res.getName() + "'",
				res.equals(IndexFactory.class));

		// check the default MapperFactory
		res = model.getIntervalModel().getMapperFactory().getClass();
		assertTrue("Instance of '" + res.getName() + "'",
				res.equals(MapperFactory.class));
	}

	/**
	 * Tests the replacement of the default {@code IndexFactory}.
	 */
	@Test
	public void testChangedFactories() {
		final TidaModel model = getTidaModel("/net/meisen/dissertation/config/xslt/configChangeFactories.xml");
		Class<?> res;

		res = model.getMetaDataModel().getIndexFactory().getClass();
		assertTrue("Instance of '" + res.getName() + "'",
				res.equals(MockIndexFactory.class));

		res = model.getIntervalModel().getMapperFactory().getClass();
		assertTrue("Instance of '" + res.getName() + "'",
				res.equals(MockMapperFactory.class));
	}

	/**
	 * Tests the implementation of {@link MetaDataModel#getDescriptorModels()}
	 * and {@link MetaDataModel#getDescriptorModel(String)}.
	 */
	@Test
	public void testGetDescriptorModel() {
		final MetaDataModel model = getMetaDataModel("/net/meisen/dissertation/config/xslt/descriptors.xml");

		assertEquals(2, model.getDescriptorModels().size());
		assertNotNull(model.getDescriptorModel("D1"));
		assertNotNull(model.getDescriptorModel("D2"));
		assertNull(model.getDescriptorModel("NOTKNOWN"));
		assertNull(model.getDescriptorModel("INVALID"));
		assertNull(model.getDescriptorModel(null));
		assertNull(model.getDescriptorModel(""));
	}

	/**
	 * Tests the implementation of
	 * {@link MetaDataModel#getDescriptor(String, Object)} and
	 * {@link MetaDataModel#getDescriptorByValue(String, Object)}.
	 */
	@Test
	public void testGetDescriptor() {
		final MetaDataModel model = getMetaDataModel("/net/meisen/dissertation/config/xslt/descriptors.xml");

		assertEquals(5, model.getDescriptors().size());

		// check the DescriptorModels
		DescriptorModel<?> desModel;
		desModel = model.getDescriptorModel("D1");
		assertNotNull(desModel);
		assertEquals(2, desModel.size());
		assertNotNull(model.getDescriptorByValue("D1", "IchHabeHunger"));
		assertNotNull(model.getDescriptorByValue("D1", ""));

		desModel = model.getDescriptorModel("D2");
		assertNotNull(desModel);
		assertEquals(3, desModel.size());
		assertNotNull(model.getDescriptor("D2", 1));
		assertNotNull(model.getDescriptor("D2", 2));
		assertNotNull(model.getDescriptor("D2", 3));
	}

	/**
	 * Tests the reading of the {@code OfflineMode}.
	 */
	@Test
	public void testOfflineMode() {
		final TidaModel model = getTidaModel("/net/meisen/dissertation/config/xslt/configOfflineMode.xml");

		assertEquals(OfflineMode.TRUE, model.getOfflineMode());

		final MetaDataModel metaModel = model.getMetaDataModel();
		assertNotNull(metaModel);
		assertEquals(OfflineMode.TRUE, metaModel.getOfflineMode());

		final DataModel dataModel = model.getDataModel();
		assertNotNull(dataModel);
		assertEquals(OfflineMode.TRUE, dataModel.getOfflineMode());

		final Collection<DescriptorModel<?>> descModels = metaModel
				.getDescriptorModels();
		assertEquals(2, descModels.size());
		for (final DescriptorModel<?> descModel : descModels) {
			assertEquals(OfflineMode.TRUE, descModel.getOfflineMode());
		}
	}

	/**
	 * Tests the implementation of an extension.
	 */
	@Test
	public void testDescriptorModelExtension() {
		final MetaDataModel model = getMetaDataModel("/net/meisen/dissertation/config/xslt/extendedDescriptors.xml");

		assertNotNull(model.getDescriptorModel("D4"));
		assertEquals(3, model.getDescriptors().size());
	}

	/**
	 * Tests an invalid definition of a {@code DataRetriever}.
	 */
	@Test
	public void testInvalidDataRetriever() {
		thrown.expect(DbDataRetrieverException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("Unable to retrieve a new connection to the specified database"));

		getMetaDataModel("/net/meisen/dissertation/config/xslt/invalidDataRetriever.xml");
	}

	/**
	 * Tests the usage of an invalid {@code DataRetriever} reference.
	 */
	@Test
	public void testInvalidDataRetrieverReference() {
		thrown.expect(NoSuchBeanDefinitionException.class);
		thrown.expectMessage(JUnitMatchers.containsString("No bean named"));

		getMetaDataModel("/net/meisen/dissertation/config/xslt/invalidDataRetrieverReference.xml");
	}

	/**
	 * Tests the creation of an empty {@code DataStructure} using the xslt.
	 */
	@Test
	public void testEmptyStructure() {
		final DataStructure structure = getDataStructure("/net/meisen/dissertation/config/xslt/emptyStructure.xml");
		assertEquals(0, structure.getSize());
	}

	/**
	 * Tests the creation of a {@code DataStructure} using the xslt.
	 */
	@Test
	public void testStructure() {
		final DataStructure s = getDataStructure("/net/meisen/dissertation/config/xslt/structure.xml");
		assertEquals(6, s.getSize());

		// check the read types
		assertEquals(4, s.getEntriesByClass(MetaStructureEntry.class).size());
		assertEquals(2, s.getEntriesByClass(IntervalStructureEntry.class)
				.size());

		// get the meta-values
		for (final MetaStructureEntry e : s
				.getEntriesByClass(MetaStructureEntry.class)) {
			if ("meta1".equals(e.getName())) {
				assertEquals("D1", e.getDescriptorModel());
				assertEquals(-1, e.getPosition());
			} else if ("meta2".equals(e.getName())) {
				assertEquals("D2", e.getDescriptorModel());
				assertEquals(2, e.getPosition());
			} else if ("meta3".equals(e.getName())) {
				assertEquals("D3", e.getDescriptorModel());
				assertEquals(-1, e.getPosition());
			} else if ("meta4".equals(e.getName())) {
				assertEquals("D4", e.getDescriptorModel());
				assertEquals(-1, e.getPosition());
			} else {
				fail("Entry with invalid name '" + e.getName() + "' found");
			}
		}

		// get the interval
		for (final IntervalStructureEntry e : s
				.getEntriesByClass(IntervalStructureEntry.class)) {
			if ("intervalend".equals(e.getName())) {
				assertEquals(-1, e.getPosition());
				assertEquals(IntervalType.END, e.getIntervalType());
			} else if (e.getPosition() == 1) {
				assertNull(e.getName());
				assertEquals(IntervalType.START, e.getIntervalType());
			} else {
				fail("Entry with invalid name '" + e.getName()
						+ "' with position '" + e.getPosition() + "' found");
			}
		}
	}

	/**
	 * Tests the definition of a {@code DataStructure} element without any
	 * binding, i.e. without a name or a position
	 */
	@Test
	public void testStructureWithoutBinding() {
		thrown.expect(RuntimeException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("xslt could not transform"));

		getDataStructure("/net/meisen/dissertation/config/xslt/invalidStructureWithoutBinding.xml");
	}

	/**
	 * Tests the retrieval of no defined data.
	 */
	@Test
	public void testNoData() {
		final DataModel model = getDataModel("/net/meisen/dissertation/config/xslt/noDataSets.xml");
		final Iterator<IDataRecord> it = model.iterator();

		assertFalse(it.hasNext());
	}

	/**
	 * Tests the retrieval of a {@code DataModel} which consists of
	 * {@code SingleStaticDataSet} instances.
	 * 
	 * @throws TransformationFailedException
	 *             if the xml cannot be read
	 * @throws ParseException
	 *             if a comparison value cannot be parsed
	 * 
	 * @see SingleStaticDataSet
	 */
	@Test
	public void testSingleData() throws TransformationFailedException,
			ParseException {
		final DataModel model = getDataModel("/net/meisen/dissertation/config/xslt/singleStaticDataSets.xml");
		final IClosableIterator<IDataRecord> it = model.iterator();

		// count the entries
		int count = 0;
		while (it.hasNext()) {
			final IDataRecord record = it.next();
			count++;

			if (count == 1) {
				assertEquals(3, record.getSize());

				assertEquals(
						Dates.parseDate("20.01.1981 08:00", "dd.MM.yyyy HH:mm"),
						record.getValue(1));
				assertEquals("Tobias", record.getValue(2));
				assertEquals(1, record.getValue(3));

				assertFalse(record.hasNamedValue("status"));
			} else if (count == 2) {
				assertEquals(3, record.getSize());

				assertEquals(
						Dates.parseDate("20.01.1981 08:07", "dd.MM.yyyy HH:mm"),
						record.getValue(1));
				assertEquals("Philipp", record.getValue(2));
				assertEquals("1", record.getValue(3));

				assertTrue(record.hasNamedValue("status"));
				assertEquals("1", record.getValue("status"));
			} else if (count == 3) {
				assertEquals(8, record.getSize());

				assertEquals(12.5d, record.getValue("double"));
				assertEquals("Philipp", record.getValue("string"));
				assertEquals(1, record.getValue("integer"));
				assertEquals(Dates.parseDate("20.01.1981", "dd.MM.yyyy"),
						record.getValue("date"));

				assertEquals(1l, record.getValue("intLong"));
				assertEquals(1.0d, (Double) record.getValue("intDouble"), 0.0d);
				assertEquals(new BigInteger("1"), record.getValue("intBigInt"));
				assertEquals(new BigDecimal("1"), record.getValue("intBigDec"));
			} else {
				fail("Count cannot have a value of '" + count + "'");
			}
		}

		// cleanUp
		it.close();
	}

	/**
	 * Tests the implementation of a {@code DataModel} using two
	 * {@code DataRetrieverDataSet} instances.
	 * 
	 * @throws TransformationFailedException
	 *             if the xml cannot be read
	 * @throws ParseException
	 *             if a comparison value cannot be parsed
	 * @throws IOException
	 *             if the database cannot be opened
	 * 
	 * @see DataRetrieverDataSet
	 */
	@Test
	public void testDbData() throws TransformationFailedException,
			ParseException, IOException {

		// we need a running database now
		getDb("/net/meisen/dissertation/impl/hsqldbs/tidaTestData.zip");

		final DataModel model = getDataModel("/net/meisen/dissertation/config/xslt/dbDataSets.xml");
		final IClosableIterator<IDataRecord> it = model.iterator();

		// get the retrievers
		assertEquals(1, model.sizeOfRetrievers());
		assertNotNull(model.getDataRetriever("db_test"));
		assertNull(model.getDataRetriever("another_id"));

		// get the expected
		final Set<Integer> expected = new HashSet<Integer>();
		for (int i = 1; i <= 10000; i = i == 9 ? 9991 : i + 1) {
			expected.add(i);
		}

		// check the expected values
		while (it.hasNext()) {
			final IDataRecord record = it.next();
			final Object val = record.getValue("COUNTER");

			assertTrue(val + " not found", expected.remove(val));
			assertTrue(record.hasNamedValue("FIXED"));
			assertEquals("FIXED VALUE", record.getValue("FIXED"));
		}
		assertEquals(0, expected.size());

		// cleanUp
		it.close();
	}

	/**
	 * Tests the default {@code TimelineDefinition}.
	 */
	@Test
	public void testDefaultTimelineOfIntervalModel() {
		final Date now = Dates.truncDate(Dates.now());
		final IntervalModel model = getIntervalModel("/net/meisen/dissertation/config/simplestModel.xml");
		final TimelineDefinition def = model.getTimelineDefinition();

		// check the correctness of the default timeline
		assertEquals(Date.class, def.getType());
		assertEquals(Classes.getClass(DefaultValues.getDefaultGranularity()),
				def.getGranularity().getClass());
		assertEquals(now.getTime(), def.<Date> getStart().getTime(), 1000);
		assertEquals(DateFormat.YEAR.modify(now, 1).getTime(), def
				.<Date> getEnd().getTime(), 1000);
	}

	/**
	 * Tests the reading of a {@code TimelineDefinition} using date values for
	 * start and end.
	 */
	@Test
	public void testTimelineWithStartEndDate() {
		final IntervalModel model = getIntervalModel("/net/meisen/dissertation/config/xslt/timelineWithStartEnd.xml");
		final TimelineDefinition def = model.getTimelineDefinition();

		assertEquals(Date.class, def.getType());
		assertEquals(Minute.instance(), def.getGranularity());
		assertEquals(Dates.isDate("01.04.2010", Dates.GENERAL_TIMEZONE),
				def.getStart());
		assertEquals(Dates.isDate("29.04.2010", Dates.GENERAL_TIMEZONE),
				def.getEnd());
	}

	/**
	 * Tests the reading of a {@code TimelineDefinition} using long values for
	 * start and end.
	 */
	@Test
	public void testTimelineWithStartEndLong() {
		final IntervalModel model = getIntervalModel("/net/meisen/dissertation/config/xslt/timelineWithStartEndLongs.xml");
		final TimelineDefinition def = model.getTimelineDefinition();

		assertEquals(Long.class, def.getType());
		assertEquals(DefaultValues.getDefaultGranularity(), def
				.getGranularity().getClass().getName());
		assertEquals(1500l, def.getStart());
		assertEquals(10000l, def.getEnd());
	}

	/**
	 * Tests the usage of a {@code TimelineDefinition} with just a start.
	 */
	@Test
	public void testTimelineWithStartOnly() {
		final IntervalModel model = getIntervalModel("/net/meisen/dissertation/config/xslt/timelineWithStartOnly.xml");
		final TimelineDefinition def = model.getTimelineDefinition();

		assertEquals(Date.class, def.getType());
		assertEquals(Second.instance(), def.getGranularity());
		assertEquals(
				Dates.isDate("01.04.2010 08:07:55", Dates.GENERAL_TIMEZONE),
				def.getStart());
		assertEquals(
				Dates.isDate("01.04.2011 08:07:55", Dates.GENERAL_TIMEZONE),
				def.getEnd());
	}

	/**
	 * Tests the usage of a {@code TimelineDefinition} with just an end.
	 */
	@Test
	public void testTimelineWithEndOnly() {
		final IntervalModel model = getIntervalModel("/net/meisen/dissertation/config/xslt/timelineWithEndOnly.xml");
		final TimelineDefinition def = model.getTimelineDefinition();

		assertEquals(Date.class, def.getType());
		assertEquals(Hour.instance(), def.getGranularity());
		assertEquals(
				Dates.isDate("28.02.2007 08:00:12", Dates.GENERAL_TIMEZONE),
				def.getStart());
		assertEquals(
				Dates.isDate("29.02.2008 08:00:12", Dates.GENERAL_TIMEZONE),
				def.getEnd());
	}

	/**
	 * Tests the usage of a {@code TimelineDefinition} with a duration.
	 */
	@Test
	public void testTimelineWithDuration() {
		final IntervalModel model = getIntervalModel("/net/meisen/dissertation/config/xslt/timelineWithDuration.xml");
		final TimelineDefinition def = model.getTimelineDefinition();

		assertEquals(Date.class, def.getType());
		assertEquals(Minute.instance(), def.getGranularity());
		assertEquals(Dates.isDate("20.01.1981", Dates.GENERAL_TIMEZONE),
				def.getStart());
		assertEquals(
				Dates.isDate("20.01.1981 01:40:00", Dates.GENERAL_TIMEZONE),
				def.getEnd());
	}

	/**
	 * Tests the usage of a {@code TimelineDefinition} with a duration and a
	 * modified granularity for the duration.
	 */
	@Test
	public void testTimelineWithDurationAndDurationGranularity() {
		final IntervalModel model = getIntervalModel("/net/meisen/dissertation/config/xslt/timelineWithDurationAndDurationGranularity.xml");
		final TimelineDefinition def = model.getTimelineDefinition();

		assertEquals(Date.class, def.getType());
		assertEquals(Minute.instance(), def.getGranularity());
		assertEquals(Dates.isDate("20.01.1981", Dates.GENERAL_TIMEZONE),
				def.getStart());
		assertEquals(Dates.isDate("20.01.2081", Dates.GENERAL_TIMEZONE),
				def.getEnd());
	}

	/**
	 * Tests the definition of a mixed timeline, i.e. a date and long value in
	 * use.
	 */
	@Test
	public void testMixedTimeline() {
		final IntervalModel model = getIntervalModel("/net/meisen/dissertation/config/xslt/timelineMixed.xml");
		final TimelineDefinition def = model.getTimelineDefinition();

		assertEquals(Date.class, def.getType());
		assertEquals(DefaultValues.getDefaultGranularity(), def
				.getGranularity().getClass().getName());
		assertEquals(
				Dates.isDate("22.08.2013 15:00:00", Dates.GENERAL_TIMEZONE),
				def.getStart());
		assertEquals(
				Dates.isDate("22.08.2013 15:10:00", Dates.GENERAL_TIMEZONE),
				def.getEnd());
	}

	/**
	 * Tests the created {@code MetaDataModel}.
	 */
	@Test
	public void testFullModelCreation() {

		// get the model
		final MetaDataModel m = getMetaDataModel("/net/meisen/dissertation/config/fullModel.xml");
		assertNotNull(m);

		// check the resources and descriptors
		Collection<Descriptor<?, ?, ?>> des;
		des = m.getDescriptors();
		assertEquals(21, des.size());

		des = m.getDescriptorsByClass(Descriptor.class);
		assertEquals(21, des.size());
		des = m.getDescriptorsByClass(Object.class);
		assertEquals(21, des.size());
		des = m.getDescriptorsByClass(ResourceDescriptor.class);
		assertEquals(7, des.size());

		// check all the created identifiers of the resources
		final Set<UUID> uuids = new HashSet<UUID>();
		for (final Descriptor<?, ?, ?> r : des) {
			assertTrue(r.getId().getClass().getName(),
					r.getId() instanceof UUID);
			assertTrue(uuids.add((UUID) r.getId()));
		}

		// check all the created identifiers of the descriptors
		des = m.getDescriptorsByClass(ListDescriptor.class);
		assertEquals(3, des.size());
		final Set<String> expectedValues = new HashSet<String>(Arrays.asList(
				"A", "B", "C", "D", "E", "F", "G", "H", "I"));
		final Set<Long> longids = new HashSet<Long>();
		for (final Descriptor<?, ?, ?> d : des) {
			assertTrue(d.getId().getClass().getName(),
					d.getId() instanceof Long);
			assertTrue(longids.add((Long) d.getId()));

			@SuppressWarnings("unchecked")
			final ListDescriptor<Long> myOwn = (ListDescriptor<Long>) d;
			assertEquals(d.getModelName(), "D4");

			// check the values
			final List<String> values = myOwn.getValueList();
			assertTrue(expectedValues.removeAll(values));
		}

		// check if all the expected values were retrieved
		assertEquals(0, expectedValues.size());
	}

	/**
	 * Tests if the full model configuration with external data-sources can be
	 * read.
	 * 
	 * @throws IOException
	 *             if the file cannot be read
	 */
	@Test
	public void testFullModelDataFromExternalCreation() throws IOException {

		// we need a running database now
		getDb("/net/meisen/dissertation/impl/hsqldbs/tidaTestData.zip");

		// get the model
		final MetaDataModel m = getMetaDataModel("/net/meisen/dissertation/config/fullModelDataFromExternal.xml");
		assertNotNull(m);

		// check the descriptors
		assertEquals(
				4,
				m.getDescriptorsByClass(GeneralDescriptor.class,
						IntegerDescriptor.class, LongDescriptor.class,
						DoubleDescriptor.class).size());
		assertNotNull(m.getDescriptorByValue("D1", "FIXED VALUE"));
		assertNotNull(m.getDescriptorByValue("D2", 2));
		assertNotNull(m.getDescriptorByValue("D3", "Some Value"));
		assertNotNull(m.getDescriptorByValue("D4", 1.0));

		// check the resources
		assertEquals(10002, m.getDescriptorsByClass(ResourceDescriptor.class)
				.size());
		for (int i = 1; i <= 10000; i++) {
			assertNotNull(m.getDescriptorByValue("R2", "" + i));
		}
		assertNotNull(m.getDescriptorByValue("R1", "Edison"));
		assertNotNull(m.getDescriptorByValue("R3", "NoValue"));
	}

	/**
	 * Helper method to match documents against regular expressions lines.
	 * 
	 * @param output
	 *            the document to be matched
	 * @param lines
	 *            the lines (as regular expressions) which must be in the
	 *            sequence within the document
	 * 
	 * @return {@code true} if the document matches, otherwise {@code false}
	 */
	protected boolean match(final String output, final String... lines) {

		// create the expected regular expression
		String regEx = "";
		regEx += "^.*";

		// append the lines
		for (final String line : lines) {
			regEx += "\\s*\\Q" + line + "\\E";
		}
		regEx += ".*$";

		final Pattern pattern = Pattern.compile(regEx, Pattern.DOTALL);
		final Matcher matcher = pattern.matcher(output);
		return matcher.matches();
	}

	/**
	 * Helper method which prints the passed xml to the console.
	 * 
	 * @param xml
	 *            the xml to be printed
	 * @throws TransformationFailedException
	 *             if the file cannot be transformed
	 */
	protected void printTransformation(final String xml)
			throws TransformationFailedException {
		transformer.transformFromClasspath(xml, outputStream);
	}

	/**
	 * Helper method to retrieve the generated output as string.
	 * 
	 * @return the generated output as string
	 */
	protected String getOutput() {
		try {
			return new String(outputStream.toByteArray(), "UTF8");
		} catch (UnsupportedEncodingException e) {
			// ignore
			return null;
		}
	}
}
