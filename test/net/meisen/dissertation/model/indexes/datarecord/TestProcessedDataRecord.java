package net.meisen.dissertation.model.indexes.datarecord;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.HashSet;
import java.util.Set;

import net.meisen.dissertation.exceptions.DescriptorModelException;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IClosableIterator;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.datastructure.MetaStructureEntry;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.NullDescriptor;

import org.junit.Test;

/**
 * Tests the implementation of a {@code ProcessedDataRecord}.
 * 
 * @author pmeisen
 * 
 */
public class TestProcessedDataRecord extends LoaderBasedTest {

	/**
	 * Tests the creation of descriptors by a {@code ProcessedDataRecord}.
	 */
	@Test
	public void testDescriptorCreation() {
		final TidaModel model = m(
				"/net/meisen/dissertation/model/indexes/datarecord/processedDataCreateDescriptor.xml",
				false);

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
					it.next(), model, counter + 1);

			// the record itself should only have one descriptor
			assertEquals(1, record.getAllDescriptors().size());

			// increase the counter we processed another record
			counter++;
			assertEquals(counter, record.getId());

			// create a set of all the descriptors
			final Set<Descriptor<?, ?, ?>> curDesc = new HashSet<Descriptor<?, ?, ?>>();
			curDesc.addAll(model.getMetaDataModel().getDescriptors());

			// make sure the descriptor is created
			assertEquals(counter, curDesc.size());
		}

		// cleanup
		it.close();
		model.release(true);
	}

	/**
	 * Tests the creation of descriptors by a {@code ProcessedDataRecord}.
	 */
	@Test
	public void testDescriptorFailure() {
		final TidaModel model = m(
				"/net/meisen/dissertation/model/indexes/datarecord/processedDataFailDescriptor.xml",
				false);

		// check the pre-requirements
		assertEquals(MetaDataHandling.FAILONERROR, model.getMetaDataHandling());

		// add all the data manually
		final IClosableIterator<IDataRecord> it = model.getDataModel()
				.iterator();
		assertTrue(it.hasNext());

		// create the processed record for each record of the model
		boolean exception = false;
		try {
			new ProcessedDataRecord(it.next(), model, 1);
		} catch (final DescriptorModelException e) {
			assertTrue(e.getMessage().contains(
					"descriptor for the value 'Holger' could not be found."));
			exception = true;
		}
		assertTrue(exception);

		// cleanUp
		model.release(true);
	}

	/**
	 * Tests the creation of descriptors by a {@code ProcessedDataRecord}.
	 */
	@Test
	public void testNullDescriptor() {
		final TidaModel model = m(
				"/net/meisen/dissertation/model/indexes/datarecord/processedDataNullDescriptor.xml",
				false);

		// check the pre-requirements
		assertEquals(MetaDataHandling.HANDLEASNULL, model.getMetaDataHandling());

		// add all the data manually
		final IClosableIterator<IDataRecord> it = model.getDataModel()
				.iterator();
		assertTrue(it.hasNext());

		// create the processed record for each record of the model
		final ProcessedDataRecord record = new ProcessedDataRecord(it.next(),
				model, 1);
		assertTrue(record.getDescriptor((MetaStructureEntry) model
				.getDataStructure().getEntries().get(0)) instanceof NullDescriptor);

		// cleanUp
		model.release(true);
	}

	/**
	 * Tests the mapping of interval values using the
	 * {@link IntervalDataHandling#USEOTHER} strategy.
	 */
	@Test
	public void testIntervalMappingUsingUseOther() {
		final TidaModel model = m(
				"/net/meisen/dissertation/model/indexes/datarecord/processedDataIntervalMappingWithUseOther.xml",
				false);

		// check the pre-requirements
		assertEquals(IntervalDataHandling.USEOTHER,
				model.getIntervalDataHandling());

		final IClosableIterator<IDataRecord> it = model.getDataModel()
				.iterator();

		// check different results
		ProcessedDataRecord record;

		// an interval [110, 200] within the timeline [100, 500]
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(10, record.getStart());
		assertEquals(100, record.getEnd());

		// an interval [1000, 1010] exceeding the timeline [100, 500]
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [10, 20] undercutting the timeline [100, 500]
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [120, 110] generally invalid
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [20, 10] generally invalid, but undercutting
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [520, 510] generally invalid, but exceeding
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [null, 110] having a null start value
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(10, record.getStart());
		assertEquals(10, record.getEnd());

		// an interval [110, null] having a null end value
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(10, record.getStart());
		assertEquals(10, record.getEnd());

		// an interval [null, null] having both values null
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [null, 1000] with null as start and exceeded end
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [1000, null] with null as end and exceeded start
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [null, 10] with null as start and undercutting end
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [10, null] with null as end and undercutting start
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// cleanup
		it.close();
		model.release(true);
	}

	/**
	 * Tests the mapping of interval values using the
	 * {@link IntervalDataHandling#USEOTHER} strategy and an excluded end.
	 */
	@Test
	public void testIntervalMappingUsingUseOtherAndExcludes() {
		final TidaModel model = m(
				"/net/meisen/dissertation/model/indexes/datarecord/processedDataIntervalMappingWithUseOtherAndExcludes.xml",
				false);

		final IClosableIterator<IDataRecord> it = model.getDataModel()
				.iterator();

		// check different results
		ProcessedDataRecord record;

		// an interval [110, 200) within the timeline [100, 500]
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(10, record.getStart());
		assertEquals(99, record.getEnd());

		// an interval [1000, 1010) exceeding the timeline [100, 500]
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [10, 20) undercutting the timeline [100, 500]
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [120, 110) generally invalid
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [20, 10) generally invalid, but undercutting
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [520, 510) generally invalid, but exceeding
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [null, 110) having a null start value
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(9, record.getStart());
		assertEquals(9, record.getEnd());

		// an interval [110, null) having a null end value
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(10, record.getStart());
		assertEquals(10, record.getEnd());

		// an interval [null, null) having both values null
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [null, 1000) with null as start and exceeded end
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [1000, null) with null as end and exceeded start
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [null, 10) with null as start and undercutting end
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [10, null) with null as end and undercutting start
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// cleanup
		it.close();
		model.release(true);
	}

	/**
	 * Tests the mapping of interval values using the
	 * {@link IntervalDataHandling#BOUNDARIESWHENNULL} strategy.
	 */
	@Test
	public void testIntervalMappingUsingBoundaries() {
		final TidaModel model = m(
				"/net/meisen/dissertation/model/indexes/datarecord/processedDataIntervalMappingWithBoundaries.xml",
				false);

		// check the pre-requirements
		assertEquals(IntervalDataHandling.BOUNDARIESWHENNULL,
				model.getIntervalDataHandling());

		final IClosableIterator<IDataRecord> it = model.getDataModel()
				.iterator();

		// check different results
		ProcessedDataRecord record;

		// an interval [110, 200] within the timeline [100, 500]
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(10, record.getStart());
		assertEquals(100, record.getEnd());

		// an interval [1000, 1010] exceeding the timeline [100, 500]
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [10, 20] undercutting the timeline [100, 500]
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [120, 110] generally invalid
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [20, 10] generally invalid, but undercutting
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [520, 510] generally invalid, but exceeding
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [null, 110] having a null start value
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(0, record.getStart());
		assertEquals(10, record.getEnd());

		// an interval [110, null] having a null end value
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(10, record.getStart());
		assertEquals(400, record.getEnd());

		// an interval [null, null] having both values null
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(0, record.getStart());
		assertEquals(400, record.getEnd());

		// an interval [null, 1000] with null as start and exceeded end
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(0, record.getStart());
		assertEquals(400, record.getEnd());

		// an interval [1000, null] with null as end and exceeded start
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [null, 10] with null as start and undercutting end
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [10, null] with null as end and undercutting start
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(0, record.getStart());
		assertEquals(400, record.getEnd());

		// cleanup
		it.close();
		model.release(true);
	}

	/**
	 * Tests the mapping of interval values using the
	 * {@link IntervalDataHandling#BOUNDARIESWHENNULL} strategy and an excluded
	 * end.
	 */
	@Test
	public void testIntervalMappingUsingBoundariesAndExcludes() {
		final TidaModel model = m(
				"/net/meisen/dissertation/model/indexes/datarecord/processedDataIntervalMappingWithBoundariesAndExcludes.xml",
				false);

		// check the pre-requirements
		assertEquals(IntervalDataHandling.BOUNDARIESWHENNULL,
				model.getIntervalDataHandling());

		final IClosableIterator<IDataRecord> it = model.getDataModel()
				.iterator();

		// check different results
		ProcessedDataRecord record;

		// an interval [110, 200) within the timeline [100, 500]
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(10, record.getStart());
		assertEquals(99, record.getEnd());

		// an interval [1000, 1010) exceeding the timeline [100, 500]
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [10, 20) undercutting the timeline [100, 500]
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [120, 110) generally invalid
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [20, 10) generally invalid, but undercutting
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [520, 510) generally invalid, but exceeding
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [null, 110) having a null start value
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(0, record.getStart());
		assertEquals(9, record.getEnd());

		// an interval [110, null) having a null end value
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(10, record.getStart());
		assertEquals(400, record.getEnd());

		// an interval [null, null) having both values null
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(0, record.getStart());
		assertEquals(400, record.getEnd());

		// an interval [null, 1000) with null as start and exceeded end
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(0, record.getStart());
		assertEquals(400, record.getEnd());

		// an interval [1000, null) with null as end and exceeded start
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [null, 10) with null as start and undercutting end
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(-1, record.getStart());
		assertEquals(-1, record.getEnd());

		// an interval [10, null) with null as end and undercutting start
		assertTrue(it.hasNext());
		record = new ProcessedDataRecord(it.next(), model, 1);
		assertEquals(0, record.getStart());
		assertEquals(400, record.getEnd());

		// cleanup
		it.close();
		model.release(true);
	}
}
