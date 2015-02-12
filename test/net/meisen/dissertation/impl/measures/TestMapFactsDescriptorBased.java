package net.meisen.dissertation.impl.measures;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.util.Arrays;
import java.util.HashSet;

import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.dissertation.model.indexes.datarecord.slices.SliceWithDescriptors;
import net.meisen.dissertation.model.util.IDoubleIterator;
import net.meisen.dissertation.model.util.IIntIterator;

import org.junit.Test;

/**
 * Tests the implementation of {@code MapFactsDescriptorBased}.
 * 
 * @author pmeisen
 * 
 */
public class TestMapFactsDescriptorBased extends LoaderBasedTest {

	private TidaModel model = null;

	private MapFactsDescriptorBased createNumberMap(final int sliceId) {

		if (model == null) {
			final String xml = "/net/meisen/dissertation/impl/measures/testFactModel.xml";
			model = m(xml);
		}

		// check the amount of data
		assertEquals(5, model.getAmountOfRecords());

		// get the slice
		final SliceWithDescriptors<?>[] slices = model.getIndex()
				.getIntervalIndexSlices(sliceId, sliceId, true, true);
		assertEquals(1, slices.length);

		// create the map
		final Bitmap bmp = slices[0] == null ? null : slices[0].getBitmap();
		final FactDescriptorSet desc = slices[0] == null ? null : slices[0]
				.getDescriptors("NUMBER");
		final MapFactsDescriptorBased map = new MapFactsDescriptorBased(desc,
				model.getIndex(), bmp);
		assertFalse(map.usesArrayImplementation());

		return map;
	}

	private MapFactsDescriptorBased createNaNMap(final int sliceId) {

		if (model == null) {
			final String xml = "/net/meisen/dissertation/impl/measures/testFactModel.xml";
			model = m(xml);
		}

		// check the amount of data
		assertEquals(5, model.getAmountOfRecords());

		// get the slice
		final SliceWithDescriptors<?>[] slices = model.getIndex()
				.getIntervalIndexSlices(sliceId, sliceId, true, true);
		assertEquals(1, slices.length);

		// create the map
		final Bitmap bmp = slices[0] == null ? null : slices[0].getBitmap();
		final FactDescriptorSet desc = slices[0] == null ? null : slices[0]
				.getDescriptors("NAN");
		final MapFactsDescriptorBased map = new MapFactsDescriptorBased(desc,
				model.getIndex(), bmp);
		assertFalse(map.usesArrayImplementation());

		return map;
	}

	/**
	 * Tests an empty {@code MapFactsDescriptorBased}.
	 */
	@Test
	public void testEmpty() {
		final TidaModel model = m("/net/meisen/dissertation/impl/measures/testFactModel.xml");

		final MapFactsDescriptorBased map = new MapFactsDescriptorBased(
				new FactDescriptorSet(), model.getIndex(), model
						.getIndexFactory().createBitmap());

		assertFalse(map.recordIdsIterator().hasNext());
		assertFalse(map.iterator(true).hasNext());
		assertFalse(map.sortedIterator().hasNext());
		assertFalse(map.descSortedIterator().hasNext());

		assertEquals(0, map.amount());
		assertEquals(Double.NaN, map.getFactOfRecord(4), 0.0);
	}

	/**
	 * Tests the usage of a {@code MapFactsDescriptorBased} with the
	 * {@code testFactModel}.
	 */
	@Test
	public void testGetFactOfRecord() {
		MapFactsDescriptorBased map;

		// check the 1. slice of the model
		map = createNumberMap(1);
		assertEquals(110.0, map.getFactOfRecord(0), 0.0);
		assertEquals(100.0, map.getFactOfRecord(1), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(2), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(3), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(4), 0.0);
		assertEquals(2, map.amount());
		assertEquals(0, map.amountOfNaN());
		assertEquals(2, map.amountOfNonNaN());

		map = createNaNMap(1);
		assertEquals(Double.NaN, map.getFactOfRecord(0), 0.0);
		assertEquals(1.0, map.getFactOfRecord(1), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(2), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(3), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(4), 0.0);
		assertEquals(2, map.amount());
		assertEquals(1, map.amountOfNaN());
		assertEquals(1, map.amountOfNonNaN());

		// check the 2. slice of the model
		map = createNumberMap(2);
		assertEquals(110.0, map.getFactOfRecord(0), 0.0);
		assertEquals(100.0, map.getFactOfRecord(1), 0.0);
		assertEquals(130.0, map.getFactOfRecord(2), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(3), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(4), 0.0);
		assertEquals(3, map.amount());
		assertEquals(0, map.amountOfNaN());
		assertEquals(3, map.amountOfNonNaN());

		map = createNaNMap(2);
		assertEquals(Double.NaN, map.getFactOfRecord(0), 0.0);
		assertEquals(1.0, map.getFactOfRecord(1), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(2), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(3), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(4), 0.0);
		assertEquals(3, map.amount());
		assertEquals(2, map.amountOfNaN());
		assertEquals(1, map.amountOfNonNaN());

		// check the 3. slice of the model
		map = createNumberMap(3);
		assertEquals(Double.NaN, map.getFactOfRecord(0), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(1), 0.0);
		assertEquals(130.0, map.getFactOfRecord(2), 0.0);
		assertEquals(110.0, map.getFactOfRecord(3), 0.0);
		assertEquals(110.0, map.getFactOfRecord(4), 0.0);
		assertEquals(3, map.amount());
		assertEquals(0, map.amountOfNaN());
		assertEquals(3, map.amountOfNonNaN());

		map = createNaNMap(3);
		assertEquals(Double.NaN, map.getFactOfRecord(0), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(1), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(2), 0.0);
		assertEquals(1.0, map.getFactOfRecord(3), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(4), 0.0);
		assertEquals(3, map.amount());
		assertEquals(2, map.amountOfNaN());
		assertEquals(1, map.amountOfNonNaN());

		// check the 4. slice of the model
		map = createNumberMap(4);
		assertEquals(Double.NaN, map.getFactOfRecord(0), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(1), 0.0);
		assertEquals(130.0, map.getFactOfRecord(2), 0.0);
		assertEquals(110.0, map.getFactOfRecord(3), 0.0);
		assertEquals(110.0, map.getFactOfRecord(4), 0.0);
		assertEquals(3, map.amount());
		assertEquals(0, map.amountOfNaN());
		assertEquals(3, map.amountOfNonNaN());

		map = createNaNMap(4);
		assertEquals(Double.NaN, map.getFactOfRecord(0), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(1), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(2), 0.0);
		assertEquals(1.0, map.getFactOfRecord(3), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(4), 0.0);
		assertEquals(3, map.amount());
		assertEquals(2, map.amountOfNaN());
		assertEquals(1, map.amountOfNonNaN());

		// check the 5. slice of the model
		map = createNumberMap(5);
		assertEquals(Double.NaN, map.getFactOfRecord(0), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(1), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(2), 0.0);
		assertEquals(110.0, map.getFactOfRecord(3), 0.0);
		assertEquals(110.0, map.getFactOfRecord(4), 0.0);
		assertEquals(2, map.amount());
		assertEquals(0, map.amountOfNaN());
		assertEquals(2, map.amountOfNonNaN());

		map = createNaNMap(5);
		assertEquals(Double.NaN, map.getFactOfRecord(0), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(1), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(2), 0.0);
		assertEquals(1.0, map.getFactOfRecord(3), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(4), 0.0);
		assertEquals(2, map.amount());
		assertEquals(1, map.amountOfNaN());
		assertEquals(1, map.amountOfNonNaN());

		// check the 6. slice of the model
		map = createNumberMap(6);
		assertEquals(Double.NaN, map.getFactOfRecord(0), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(1), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(2), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(3), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(4), 0.0);
		assertEquals(0, map.amount());
		assertEquals(0, map.amountOfNaN());
		assertEquals(0, map.amountOfNonNaN());

		map = createNaNMap(6);
		assertEquals(Double.NaN, map.getFactOfRecord(0), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(1), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(2), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(3), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(4), 0.0);
		assertEquals(0, map.amount());
		assertEquals(0, map.amountOfNaN());
		assertEquals(0, map.amountOfNonNaN());
	}

	/**
	 * Tests the implementation of
	 * {@link MapFactsDescriptorBased#recordIdsIterator()}.
	 */
	@Test
	public void testRecordIdsIterator() {

		// check the iterator for records
		IIntIterator recIt;
		HashSet<Integer> expIds;
		recIt = createNumberMap(1).recordIdsIterator();
		expIds = new HashSet<Integer>(Arrays.asList(new Integer[] { 0, 1 }));
		while (recIt.hasNext()) {
			expIds.remove(recIt.next());
		}
		assertEquals(0, expIds.size());

		recIt = createNumberMap(2).recordIdsIterator();
		expIds = new HashSet<Integer>(Arrays.asList(new Integer[] { 0, 1, 2 }));
		while (recIt.hasNext()) {
			expIds.remove(recIt.next());
		}
		assertEquals(0, expIds.size());

		recIt = createNumberMap(3).recordIdsIterator();
		expIds = new HashSet<Integer>(Arrays.asList(new Integer[] { 2, 3, 4 }));
		while (recIt.hasNext()) {
			expIds.remove(recIt.next());
		}
		assertEquals(0, expIds.size());

		recIt = createNumberMap(4).recordIdsIterator();
		expIds = new HashSet<Integer>(Arrays.asList(new Integer[] { 2, 3, 4 }));
		while (recIt.hasNext()) {
			expIds.remove(recIt.next());
		}
		assertEquals(0, expIds.size());

		recIt = createNumberMap(5).recordIdsIterator();
		expIds = new HashSet<Integer>(Arrays.asList(new Integer[] { 3, 4 }));
		while (recIt.hasNext()) {
			expIds.remove(recIt.next());
		}
		assertEquals(0, expIds.size());
	}

	/**
	 * Tests the implementation of
	 * {@link MapFactsDescriptorBased#iterator(boolean)}.
	 */
	@Test
	public void testFactsIterator() {

		// check the iterator for facts
		IDoubleIterator factsIt;
		HashSet<Double> expFacts;
		factsIt = createNumberMap(1).iterator(true);
		expFacts = new HashSet<Double>(Arrays.asList(new Double[] { 110.0,
				100.0 }));
		while (factsIt.hasNext()) {
			expFacts.remove(factsIt.next());
		}
		assertEquals(0, expFacts.size());

		factsIt = createNumberMap(2).iterator(true);
		expFacts = new HashSet<Double>(Arrays.asList(new Double[] { 110.0,
				100.0, 130.0 }));
		while (factsIt.hasNext()) {
			expFacts.remove(factsIt.next());
		}
		assertEquals(0, expFacts.size());

		factsIt = createNumberMap(3).iterator(true);
		expFacts = new HashSet<Double>(Arrays.asList(new Double[] { 110.0,
				130.0 }));
		while (factsIt.hasNext()) {
			expFacts.remove(factsIt.next());
		}
		assertEquals(0, expFacts.size());

		factsIt = createNumberMap(4).iterator(true);
		expFacts = new HashSet<Double>(Arrays.asList(new Double[] { 110.0,
				130.0 }));
		while (factsIt.hasNext()) {
			expFacts.remove(factsIt.next());
		}
		assertEquals(0, expFacts.size());

		factsIt = createNumberMap(5).iterator(true);
		expFacts = new HashSet<Double>(Arrays.asList(new Double[] { 110.0 }));
		while (factsIt.hasNext()) {
			expFacts.remove(factsIt.next());
		}
		assertEquals(0, expFacts.size());

		factsIt = createNumberMap(6).iterator(false);
		assertFalse(factsIt.hasNext());
	}

	/**
	 * Tests the implementation of
	 * {@link MapFactsDescriptorBased#sortedIterator()}.
	 */
	@Test
	public void testSortedFactsIterator() {

		// check the sorted facts iterator
		IDoubleIterator sortedFactsIt;
		sortedFactsIt = createNumberMap(1).sortedIterator();
		assertEquals(100.0, sortedFactsIt.next(), 0.0);
		assertEquals(110.0, sortedFactsIt.next(), 0.0);

		sortedFactsIt = createNumberMap(2).sortedIterator();
		assertEquals(100.0, sortedFactsIt.next(), 0.0);
		assertEquals(110.0, sortedFactsIt.next(), 0.0);
		assertEquals(130.0, sortedFactsIt.next(), 0.0);

		sortedFactsIt = createNumberMap(3).sortedIterator();
		assertEquals(110.0, sortedFactsIt.next(), 0.0);
		assertEquals(110.0, sortedFactsIt.next(), 0.0);
		assertEquals(130.0, sortedFactsIt.next(), 0.0);

		sortedFactsIt = createNumberMap(4).sortedIterator();
		assertEquals(110.0, sortedFactsIt.next(), 0.0);
		assertEquals(110.0, sortedFactsIt.next(), 0.0);
		assertEquals(130.0, sortedFactsIt.next(), 0.0);

		sortedFactsIt = createNumberMap(5).sortedIterator();
		assertEquals(110.0, sortedFactsIt.next(), 0.0);
		assertEquals(110.0, sortedFactsIt.next(), 0.0);
	}

	/**
	 * Tests the implementation of
	 * {@link MapFactsDescriptorBased#descSortedIterator()}.
	 */
	@Test
	public void testDescSortedFactsIterator() {

		// check the descendant sorted facts iterator
		IDoubleIterator descSortedFactsIt;
		descSortedFactsIt = createNumberMap(1).descSortedIterator();
		assertEquals(110.0, descSortedFactsIt.next(), 0.0);
		assertEquals(100.0, descSortedFactsIt.next(), 0.0);

		descSortedFactsIt = createNumberMap(2).descSortedIterator();
		assertEquals(130.0, descSortedFactsIt.next(), 0.0);
		assertEquals(110.0, descSortedFactsIt.next(), 0.0);
		assertEquals(100.0, descSortedFactsIt.next(), 0.0);

		descSortedFactsIt = createNumberMap(3).descSortedIterator();
		assertEquals(130.0, descSortedFactsIt.next(), 0.0);
		assertEquals(110.0, descSortedFactsIt.next(), 0.0);
		assertEquals(110.0, descSortedFactsIt.next(), 0.0);

		descSortedFactsIt = createNumberMap(4).descSortedIterator();
		assertEquals(130.0, descSortedFactsIt.next(), 0.0);
		assertEquals(110.0, descSortedFactsIt.next(), 0.0);
		assertEquals(110.0, descSortedFactsIt.next(), 0.0);

		descSortedFactsIt = createNumberMap(5).descSortedIterator();
		assertEquals(110.0, descSortedFactsIt.next(), 0.0);
		assertEquals(110.0, descSortedFactsIt.next(), 0.0);
	}
}
