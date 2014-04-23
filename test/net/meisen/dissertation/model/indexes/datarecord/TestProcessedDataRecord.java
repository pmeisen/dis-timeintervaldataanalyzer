package net.meisen.dissertation.model.indexes.datarecord;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.HashSet;
import java.util.Set;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.exceptions.DescriptorModelException;
import net.meisen.dissertation.help.ExceptionBasedTest;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IClosableIterator;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.datastructure.MetaStructureEntry;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.NullDescriptor;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.After;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Tests the implementation of a {@code ProcessedDataRecord}.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TidaConfig.class)
@ContextFile("sbconfigurator-core.xml")
@RunWith(JUnitConfigurationRunner.class)
public class TestProcessedDataRecord extends ExceptionBasedTest {

	@Autowired
	private TidaModelHandler loader;

	/**
	 * Tests the creation of descriptors by a {@code ProcessedDataRecord}.
	 */
	@Test
	public void testDescriptorCreation() {
		final TidaModel model = loader
				.loadViaXslt("/net/meisen/dissertation/model/indexes/datarecord/processedDataCreateDescriptor.xml");

		// check the pre-requirements
		assertEquals(MetaDataHandling.CREATEDESCRIPTOR,
				model.getMetaDataHandling());

		// add all the data manually
		final IClosableIterator<IDataRecord> it = model.getDataModel()
				.iterator();
		int counter = 0;
		while (it.hasNext()) {

			// create the processed record for each record of the model
			final ProcessedDataRecord record = new ProcessedDataRecord(
					it.next(), model);

			// the record itself should only have one descriptor
			assertEquals(1, record.getAllDescriptors().size());

			// increase the counter we processed another record
			counter++;

			// create a set of all the descriptors
			final Set<Descriptor<?, ?, ?>> curDesc = new HashSet<Descriptor<?, ?, ?>>();
			curDesc.addAll(model.getMetaDataModel().getDescriptors());

			// make sure the descriptor is created
			assertEquals(counter, curDesc.size());
		}

		// cleanup
		it.close();
	}

	/**
	 * Tests the creation of descriptors by a {@code ProcessedDataRecord}.
	 */
	@Test
	public void testDescriptorFailure() {
		thrown.expect(DescriptorModelException.class);
		thrown.expectMessage("descriptor for the value 'Holger' could not be found.");

		final TidaModel model = loader
				.loadViaXslt("/net/meisen/dissertation/model/indexes/datarecord/processedDataFailDescriptor.xml");

		// check the pre-requirements
		assertEquals(MetaDataHandling.FAILONERROR, model.getMetaDataHandling());

		// add all the data manually
		final IClosableIterator<IDataRecord> it = model.getDataModel()
				.iterator();
		assertTrue(it.hasNext());

		// create the processed record for each record of the model
		new ProcessedDataRecord(it.next(), model);
	}

	/**
	 * Tests the creation of descriptors by a {@code ProcessedDataRecord}.
	 */
	@Test
	public void testNullDescriptor() {
		final TidaModel model = loader
				.loadViaXslt("/net/meisen/dissertation/model/indexes/datarecord/processedDataNullDescriptor.xml");

		// check the pre-requirements
		assertEquals(MetaDataHandling.HANDLEASNULL, model.getMetaDataHandling());

		// add all the data manually
		final IClosableIterator<IDataRecord> it = model.getDataModel()
				.iterator();
		assertTrue(it.hasNext());

		// create the processed record for each record of the model
		final ProcessedDataRecord record = new ProcessedDataRecord(it.next(),
				model);
		assertTrue(record.getDescriptor((MetaStructureEntry) model
				.getDataStructure().getEntries().get(0)) instanceof NullDescriptor);
	}

	/**
	 * Tests the mapping of interval values using the
	 * {@link IntervalDataHandling#USEOTHER} strategy.
	 */
	@Test
	public void testIntervalMappingUsingUseOther() {
		final TidaModel model = loader
				.loadViaXslt("/net/meisen/dissertation/model/indexes/datarecord/processedDataIntervalMappingWithUseOther.xml");

		// check the pre-requirements
		assertEquals(IntervalDataHandling.USEOTHER,
				model.getIntervalDataHandling());

		final IClosableIterator<IDataRecord> it = model.getDataModel()
				.iterator();

		// check different results
		ProcessedDataRecord record;

		// an interval [110, 200] within the timeline [100, 500]
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(10, record.getStart());
		assertEquals(100, record.getEnd());

		// an interval [1000, 1010] exceeding the timeline [100, 500]
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [10, 20] undercutting the timeline [100, 500]
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [120, 110] generally invalid
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [20, 10] generally invalid, but undercutting
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [520, 510] generally invalid, but exceeding
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [null, 110] having a null start value
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(10, record.getStart());
		assertEquals(10, record.getEnd());

		// an interval [110, null] having a null end value
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(10, record.getStart());
		assertEquals(10, record.getEnd());

		// an interval [null, null] having both values null
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [null, 1000] with null as start and exceeded end
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [1000, null] with null as end and exceeded start
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [null, 10] with null as start and undercutting end
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [10, null] with null as end and undercutting start
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// cleanup
		it.close();
	}

	/**
	 * Tests the mapping of interval values using the
	 * {@link IntervalDataHandling#BOUNDARIESWHENNULL} strategy.
	 */
	@Test
	public void testIntervalMappingUsingBoundaries() {
		final TidaModel model = loader
				.loadViaXslt("/net/meisen/dissertation/model/indexes/datarecord/processedDataIntervalMappingWithBoundaries.xml");

		// check the pre-requirements
		assertEquals(IntervalDataHandling.BOUNDARIESWHENNULL,
				model.getIntervalDataHandling());

		final IClosableIterator<IDataRecord> it = model.getDataModel()
				.iterator();

		// check different results
		ProcessedDataRecord record;

		// an interval [110, 200] within the timeline [100, 500]
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(10, record.getStart());
		assertEquals(100, record.getEnd());

		// an interval [1000, 1010] exceeding the timeline [100, 500]
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [10, 20] undercutting the timeline [100, 500]
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [120, 110] generally invalid
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [20, 10] generally invalid, but undercutting
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [520, 510] generally invalid, but exceeding
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [null, 110] having a null start value
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(0, record.getStart());
		assertEquals(10, record.getEnd());

		// an interval [110, null] having a null end value
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(10, record.getStart());
		assertEquals(400, record.getEnd());

		// an interval [null, null] having both values null
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(0, record.getStart());
		assertEquals(400, record.getEnd());

		// an interval [null, 1000] with null as start and exceeded end
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(0, record.getStart());
		assertEquals(400, record.getEnd());

		// an interval [1000, null] with null as end and exceeded start
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [null, 10] with null as start and undercutting end
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [10, null] with null as end and undercutting start
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model);
		assertEquals(0, record.getStart());
		assertEquals(400, record.getEnd());

		// cleanup
		it.close();
	}

	/**
	 * Make sure all the loaded modules are unloaded.
	 */
	@After
	public void unload() {
		loader.unloadAll();
	}
}
