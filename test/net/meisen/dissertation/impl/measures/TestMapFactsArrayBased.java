package net.meisen.dissertation.impl.measures;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.util.HashSet;

import net.meisen.dissertation.help.ExceptionBasedTest;
import net.meisen.dissertation.model.util.IDoubleIterator;
import net.meisen.dissertation.model.util.IIntIterator;

import org.junit.Test;

/**
 * Tests the implementation of {@code MapFactsArrayBased}.
 * 
 * @author pmeisen
 * 
 * @see MapFactsArrayBased
 * 
 */
public class TestMapFactsArrayBased extends ExceptionBasedTest {

	/**
	 * Tests an empty {@code MapFactsArrayBased}.
	 */
	@Test
	public void testEmpty() {
		final MapFactsArrayBased map = new MapFactsArrayBased();

		assertFalse(map.recordIdsIterator().hasNext());
		assertFalse(map.iterator(true).hasNext());
		assertFalse(map.sortedIterator().hasNext());
		assertFalse(map.descSortedIterator().hasNext());

		assertEquals(0, map.amount());
		assertEquals(Double.NaN, map.getFactOfRecord(4), 0.0);
	}

	/**
	 * Tests the usage of a {@code MapFactsArrayBased}.
	 */
	@Test
	public void testUsage() {
		final MapFactsArrayBased map = new MapFactsArrayBased();

		// set some values
		map.set(5, 3.0);
		assertEquals(1, map.amount());
		assertEquals(3.0, map.getFactOfRecord(5), 0.0);
		map.set(2, 1.1);
		assertEquals(2, map.amount());
		assertEquals(1.1, map.getFactOfRecord(2), 0.0);
		map.set(1, 3.2);
		assertEquals(3, map.amount());
		assertEquals(3.2, map.getFactOfRecord(1), 0.0);

		// check the iterator for records
		final IIntIterator recIt = map.recordIdsIterator();
		final HashSet<Integer> expectedIds = new HashSet<Integer>();
		expectedIds.add(1);
		expectedIds.add(2);
		expectedIds.add(5);
		while (recIt.hasNext()) {
			expectedIds.remove(recIt.next());
		}
		assertEquals(0, expectedIds.size());

		// check the iterator for facts
		final IDoubleIterator factsIt = map.iterator(true);
		final HashSet<Double> expectedFacts = new HashSet<Double>();
		expectedFacts.add(3.0);
		expectedFacts.add(1.1);
		expectedFacts.add(3.2);
		while (factsIt.hasNext()) {
			expectedFacts.remove(factsIt.next());
		}
		assertEquals(0, expectedFacts.size());

		// check the sorted facts iterator
		final IDoubleIterator sortedFactsIt = map.sortedIterator();
		assertEquals(1.1, sortedFactsIt.next(), 0.0);
		assertEquals(3.0, sortedFactsIt.next(), 0.0);
		assertEquals(3.2, sortedFactsIt.next(), 0.0);

		// check the descending sorted facts iterator
		final IDoubleIterator descSortedFactsIt = map.descSortedIterator();
		assertEquals(3.2, descSortedFactsIt.next(), 0.0);
		assertEquals(3.0, descSortedFactsIt.next(), 0.0);
		assertEquals(1.1, descSortedFactsIt.next(), 0.0);
	}
}
