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

	private MapFactsDescriptorBased createMap(final int sliceId) {

		if (model == null) {
			final String xml = "/net/meisen/dissertation/impl/measures/testNumberModel.xml";
			model = m(xml);
		}

		// check the amount of data
		assertEquals(5, model.getAmountOfRecords());

		// create a bitmap which selects all data
		Bitmap bmp = model.getIndexFactory().createBitmap();
		bmp = bmp.invert(model.getAmountOfRecords());

		// get the slice
		final SliceWithDescriptors<?>[] slices = model.getIndex()
				.getIntervalIndexSlices(sliceId, sliceId, true, true);
		assertEquals(1, slices.length);

		// create the map
		final MapFactsDescriptorBased map = new MapFactsDescriptorBased(
				slices[0].getDescriptors("NUMBER"), model.getIndex(),
				slices[0].getBitmap());
		assertFalse(map.usesArrayImplementation());

		return map;
	}

	/**
	 * Tests an empty {@code MapFactsDescriptorBased}.
	 */
	@Test
	public void testEmpty() {
		final TidaModel model = m("/net/meisen/dissertation/impl/measures/testNumberModel.xml");

		final MapFactsDescriptorBased map = new MapFactsDescriptorBased(
				new FactDescriptorSet(), model.getIndex(), model
						.getIndexFactory().createBitmap());

		assertFalse(map.recordIdsIterator().hasNext());
		assertFalse(map.factsIterator().hasNext());
		assertFalse(map.sortedFactsIterator().hasNext());

		assertEquals(0, map.amountOfFacts());
		assertEquals(Double.NaN, map.getFactOfRecord(4), 0.0);
	}

	/**
	 * Tests the usage of a {@code MapFactsDescriptorBased} with the
	 * {@code testNumberModel}.
	 */
	@Test
	public void testUsage() {
		MapFactsDescriptorBased map;

		// check the 1. slice of the model
		map = createMap(1);
		assertEquals(110.0, map.getFactOfRecord(0), 0.0);
		assertEquals(100.0, map.getFactOfRecord(1), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(2), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(3), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(4), 0.0);
		assertEquals(2, map.amountOfFacts());

		// check the 2. slice of the model
		map = createMap(2);
		assertEquals(110.0, map.getFactOfRecord(0), 0.0);
		assertEquals(100.0, map.getFactOfRecord(1), 0.0);
		assertEquals(130.0, map.getFactOfRecord(2), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(3), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(4), 0.0);
		assertEquals(3, map.amountOfFacts());

		// check the 3. slice of the model
		map = createMap(3);
		assertEquals(Double.NaN, map.getFactOfRecord(0), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(1), 0.0);
		assertEquals(130.0, map.getFactOfRecord(2), 0.0);
		assertEquals(110.0, map.getFactOfRecord(3), 0.0);
		assertEquals(110.0, map.getFactOfRecord(4), 0.0);
		assertEquals(3, map.amountOfFacts());

		// check the 4. slice of the model
		map = createMap(4);
		assertEquals(Double.NaN, map.getFactOfRecord(0), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(1), 0.0);
		assertEquals(130.0, map.getFactOfRecord(2), 0.0);
		assertEquals(110.0, map.getFactOfRecord(3), 0.0);
		assertEquals(110.0, map.getFactOfRecord(4), 0.0);
		assertEquals(3, map.amountOfFacts());

		// check the 5. slice of the model
		map = createMap(5);
		assertEquals(Double.NaN, map.getFactOfRecord(0), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(1), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(2), 0.0);
		assertEquals(110.0, map.getFactOfRecord(3), 0.0);
		assertEquals(110.0, map.getFactOfRecord(4), 0.0);
		assertEquals(2, map.amountOfFacts());

		// check the iterator for records
		IIntIterator recIt;
		HashSet<Integer> expIds;
		recIt = createMap(1).recordIdsIterator();
		expIds = new HashSet<Integer>(Arrays.asList(new Integer[] { 0, 1 }));
		while (recIt.hasNext()) {
			expIds.remove(recIt.next());
		}
		assertEquals(0, expIds.size());

		recIt = createMap(2).recordIdsIterator();
		expIds = new HashSet<Integer>(Arrays.asList(new Integer[] { 0, 1, 2 }));
		while (recIt.hasNext()) {
			expIds.remove(recIt.next());
		}
		assertEquals(0, expIds.size());

		recIt = createMap(3).recordIdsIterator();
		expIds = new HashSet<Integer>(Arrays.asList(new Integer[] { 2, 3, 4 }));
		while (recIt.hasNext()) {
			expIds.remove(recIt.next());
		}
		assertEquals(0, expIds.size());

		recIt = createMap(4).recordIdsIterator();
		expIds = new HashSet<Integer>(Arrays.asList(new Integer[] { 2, 3, 4 }));
		while (recIt.hasNext()) {
			expIds.remove(recIt.next());
		}
		assertEquals(0, expIds.size());

		recIt = createMap(5).recordIdsIterator();
		expIds = new HashSet<Integer>(Arrays.asList(new Integer[] { 3, 4 }));
		while (recIt.hasNext()) {
			expIds.remove(recIt.next());
		}
		assertEquals(0, expIds.size());

		// check the iterator for facts
		IDoubleIterator factsIt;
		HashSet<Double> expFacts;
		factsIt = createMap(1).factsIterator();
		expFacts = new HashSet<Double>(Arrays.asList(new Double[] { 110.0,
				100.0 }));
		while (factsIt.hasNext()) {
			expFacts.remove(factsIt.next());
		}
		assertEquals(0, expFacts.size());

		factsIt = createMap(2).factsIterator();
		expFacts = new HashSet<Double>(Arrays.asList(new Double[] { 110.0,
				100.0, 130.0 }));
		while (factsIt.hasNext()) {
			expFacts.remove(factsIt.next());
		}
		assertEquals(0, expFacts.size());

		factsIt = createMap(3).factsIterator();
		expFacts = new HashSet<Double>(Arrays.asList(new Double[] { 110.0,
				130.0 }));
		while (factsIt.hasNext()) {
			expFacts.remove(factsIt.next());
		}
		assertEquals(0, expFacts.size());

		factsIt = createMap(4).factsIterator();
		expFacts = new HashSet<Double>(Arrays.asList(new Double[] { 110.0,
				130.0 }));
		while (factsIt.hasNext()) {
			expFacts.remove(factsIt.next());
		}
		assertEquals(0, expFacts.size());

		factsIt = createMap(5).factsIterator();
		expFacts = new HashSet<Double>(Arrays.asList(new Double[] { 110.0 }));
		while (factsIt.hasNext()) {
			expFacts.remove(factsIt.next());
		}
		assertEquals(0, expFacts.size());

		// check the sorted facts iterator
		IDoubleIterator sortedFactsIt;
		sortedFactsIt = createMap(1).sortedFactsIterator();
		assertEquals(100.0, sortedFactsIt.next(), 0.0);
		assertEquals(110.0, sortedFactsIt.next(), 0.0);

		sortedFactsIt = createMap(2).sortedFactsIterator();
		assertEquals(100.0, sortedFactsIt.next(), 0.0);
		assertEquals(110.0, sortedFactsIt.next(), 0.0);
		assertEquals(130.0, sortedFactsIt.next(), 0.0);

		sortedFactsIt = createMap(3).sortedFactsIterator();
		assertEquals(110.0, sortedFactsIt.next(), 0.0);
		assertEquals(110.0, sortedFactsIt.next(), 0.0);
		assertEquals(130.0, sortedFactsIt.next(), 0.0);

		sortedFactsIt = createMap(4).sortedFactsIterator();
		assertEquals(110.0, sortedFactsIt.next(), 0.0);
		assertEquals(110.0, sortedFactsIt.next(), 0.0);
		assertEquals(130.0, sortedFactsIt.next(), 0.0);

		sortedFactsIt = createMap(5).sortedFactsIterator();
		assertEquals(110.0, sortedFactsIt.next(), 0.0);
		assertEquals(110.0, sortedFactsIt.next(), 0.0);
	}
}
