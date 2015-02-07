package net.meisen.dissertation.impl.dataintegration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;

import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.impl.datasets.SingleStaticDataSet;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.general.genmisc.exceptions.catalog.DefaultLocalizedExceptionCatalog;
import net.meisen.general.genmisc.exceptions.registry.DefaultExceptionRegistry;
import net.meisen.general.genmisc.types.Objects;

import org.junit.Test;

/**
 * Tests the implementation of the {@code ScriptPreProcessor}.
 * 
 * @author pmeisen
 * 
 */
public class TestScriptPreProcessor extends LoaderBasedTest {

	/**
	 * Tests the identity mapping.
	 */
	@Test
	public void testIdentityPreProcessing() {
		String script = "";
		script += "var result = raw;";

		final ScriptPreProcessor processor = create(script);

		// use the processor
		final SingleStaticDataSet rec = new SingleStaticDataSet("TEST");
		assertEquals(rec, processor.process(rec));
	}

	/**
	 * Tests the usage of the pre-processor to combine to old values.
	 */
	@Test
	public void testCombinePreProcessing() {
		String script = "";
		script += "var result = new net.meisen.dissertation.impl.dataintegration.PreProcessedDataRecord(raw);";
		script += "result.setValue('NAME4', raw.getValue('NAME1') + ' ' + raw.getValue('NAME2'));";

		final ScriptPreProcessor processor = create(script);

		// use the processor
		final Map<String, Object> map = new LinkedHashMap<String, Object>();
		map.put("NAME1", 5);
		map.put("NAME2", 10.5);
		map.put("NAME3", "STRING");
		final IDataRecord rec = processor.process(new SingleStaticDataSet(map));
		assertEquals(5, rec.getValue("NAME1"));
		assertEquals(10.5, rec.getValue("NAME2"));
		assertEquals("STRING", rec.getValue("NAME3"));
		assertEquals("5 10.5", rec.getValue("NAME4"));
	}

	/**
	 * Tests the usage of the {@code ScriptPreProcessor} within a
	 * {@code TidaModel}.
	 */
	@Test
	public void testUsageInModel() {
		final TidaModel model = m(
				"/net/meisen/dissertation/impl/dataintegration/testScriptModel.xml",
				true);

		final MetaDataModel metaModel = model.getMetaDataModel();
		DescriptorModel<?> descModel;

		// check the newly created MORESCREAMS
		descModel = metaModel.getDescriptorModel("MORESCREAMS");
		assertDescriptors(descModel.getAllDescriptors(), 1, 4, 13);

		// check the newly created CONST
		descModel = metaModel.getDescriptorModel("CONST");
		assertDescriptors(descModel.getAllDescriptors(), "MYCONSTANT");

		// check the newly created SCREAMCATEGORY
		descModel = metaModel.getDescriptorModel("SCREAMCATEGORY");
		assertDescriptors(descModel.getAllDescriptors(), "MANY", "NONE");

		// just validate some index-record
		final IDataRecord record = model.getIndex().getRecord(0);

		assertEquals(0, record.getValue(1));
		assertTrue(record.getValue(2) instanceof Date);
		assertTrue(record.getValue(3) instanceof Date);
		assertEquals(0, record.getValue(4));
		assertEquals("Aachen", record.getValue(5));
		assertEquals("Tobias", record.getValue(6));
		assertEquals("MYCONSTANT", record.getValue(7));
		assertEquals("NONE", record.getValue(8));
		assertEquals(1, record.getValue(9));

		assertEquals("[ID]", record.getName(1));
		assertEquals("[START]", record.getName(2));
		assertEquals("[END]", record.getName(3));
		assertEquals("SCREAMS", record.getName(4));
		assertEquals("LOCATION", record.getName(5));
		assertEquals("PERSON", record.getName(6));
		assertEquals("CONST", record.getName(7));
		assertEquals("SCREAMCATEGORY", record.getName(8));
		assertEquals("MORESCREAMS", record.getName(9));

		assertEquals(1, record.getPosition("[ID]"));
		assertEquals(2, record.getPosition("[START]"));
		assertEquals(3, record.getPosition("[END]"));
		assertEquals(4, record.getPosition("SCREAMS"));
		assertEquals(5, record.getPosition("LOCATION"));
		assertEquals(6, record.getPosition("PERSON"));
		assertEquals(7, record.getPosition("CONST"));
		assertEquals(8, record.getPosition("SCREAMCATEGORY"));
		assertEquals(9, record.getPosition("MORESCREAMS"));
	}

	/**
	 * Helper method to validate the values of a descriptor.
	 * 
	 * @param descriptors
	 *            the descriptors found
	 * @param values
	 *            the descriptors expected (values)
	 */
	protected void assertDescriptors(final Collection<?> descriptors,
			final Object... values) {
		assertEquals(descriptors.size(), values.length);

		for (final Object o : descriptors) {
			assertTrue(o instanceof Descriptor);
			final Descriptor<?, ?, ?> descriptor = (Descriptor<?, ?, ?>) o;
			final Object descValue = descriptor.getValue();

			boolean found = false;
			for (final Object value : values) {
				if (Objects.equals(descValue, value)) {
					if (value != null) {
						assertEquals(descValue.getClass(), value.getClass());
					}
					found = true;
					break;
				}
			}
			assertTrue(descValue + " not found.", found);
		}
	}

	/**
	 * Helper method to create an instance for a script.
	 * 
	 * @param script
	 *            the script to be bound
	 * 
	 * @return the created {@code ScriptPreProcessor}
	 */
	protected ScriptPreProcessor create(final String script) {
		final DefaultExceptionRegistry reg = new DefaultExceptionRegistry();
		reg.addExceptionCatalogByName(ScriptPreProcessorException.class,
				DefaultLocalizedExceptionCatalog.class.getName());
		final ScriptPreProcessor processor = new ScriptPreProcessor();
		processor.setExceptionRegistry(reg);

		final ScriptPreProcessorConfig config = new ScriptPreProcessorConfig(
				"javascript");

		// initialize the processor
		config.setScript(script);
		processor.setConfig(config);

		return processor;
	}
}
