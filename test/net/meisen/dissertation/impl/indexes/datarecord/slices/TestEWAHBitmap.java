package net.meisen.dissertation.impl.indexes.datarecord.slices;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.junit.Test;

/**
 * Check the implementation of {@code EWAHBitmap}.
 * 
 * @author pmeisen
 * 
 */
public class TestEWAHBitmap {

	/**
	 * Tests the not implementation using a negative position.
	 */
	@Test
	public void testNegativePosition() {
		final EWAHBitmap bitmap = new EWAHBitmap();
		EWAHBitmap result;

		// get an invalid position
		result = bitmap.invert(-1);
		assertEquals(0, result.getIds().length);
	}

	/***
	 * Tests the not implementation for the zero position.
	 */
	@Test
	public void testInvertOneBit() {
		final EWAHBitmap bitmap = new EWAHBitmap();
		EWAHBitmap result;

		// set the first value
		bitmap.set(0);

		// make sure no value is set
		result = bitmap.invert(0);
		assertEquals(0, result.getIds().length);
	}

	/**
	 * Tests a simple inversion (i.e. not).
	 */
	@Test
	public void testSimpleInvert() {
		final EWAHBitmap bitmap = new EWAHBitmap();
		EWAHBitmap result;

		// set the first value
		bitmap.set(1, 5, 9);

		// make sure no value is set
		result = bitmap.invert(10);
		assertEquals(8, result.getIds().length);
		assertTrue(Arrays.binarySearch(result.getIds(), 0) > -1);
		assertTrue(Arrays.binarySearch(result.getIds(), 2) > -1);
		assertTrue(Arrays.binarySearch(result.getIds(), 3) > -1);
		assertTrue(Arrays.binarySearch(result.getIds(), 4) > -1);
		assertTrue(Arrays.binarySearch(result.getIds(), 6) > -1);
		assertTrue(Arrays.binarySearch(result.getIds(), 7) > -1);
		assertTrue(Arrays.binarySearch(result.getIds(), 8) > -1);
		assertTrue(Arrays.binarySearch(result.getIds(), 10) > -1);
	}

	/**
	 * Tests the full inversion of an empty bitmap.
	 */
	@Test
	public void testFullInvertToEmpty() {
		final EWAHBitmap bitmap = new EWAHBitmap();
		EWAHBitmap result;

		final int max = 10000000;

		// set the first value
		final int[] resultIds = new int[max + 1];
		for (int i = 0; i <= max; i++) {
			resultIds[i] = i;
		}
		bitmap.set(resultIds);

		// make sure no value is set
		result = bitmap.invert(max);
		assertEquals(0, result.getIds().length);
	}

	/**
	 * Tests the full inversion of a full bitmap.
	 */
	@Test
	public void testFullInvertToFull() {
		final EWAHBitmap bitmap = new EWAHBitmap();
		EWAHBitmap result;

		final int max = 10000000;

		// make sure no value is set
		result = bitmap.invert(max);
		final int[] resultIds = result.getIds();
		assertEquals(max + 1, resultIds.length);
		for (int i = 0; i <= max; i++) {
			assertTrue(Arrays.binarySearch(resultIds, i, i + 1, i) > -1);
		}
	}

	/**
	 * Tests a partial inversion.
	 */
	@Test
	public void testPartialInvert() {

		final EWAHBitmap bitmap = new EWAHBitmap();
		EWAHBitmap result;

		final int max = 9;
		final int halfPos = Math.round((max + 1) / 2) - 1;
		for (int i = 0; i <= max; i++) {
			bitmap.set(i);
		}

		// invert the halfPos
		result = bitmap.invert(halfPos);
		final int[] resultIds = result.getIds();

		// check the result
		assertEquals(halfPos + 1, resultIds.length);
		for (int i = 0; i <= max; i++) {
			if (i <= halfPos) {
				assertFalse(Arrays.binarySearch(resultIds, i) > -1);
			} else {
				assertTrue(Arrays.binarySearch(resultIds, i) > -1);
			}
		}
	}

	/**
	 * Checks the inversion using a overflow value.
	 */
	@Test
	public void testOverflowInvert() {

		final EWAHBitmap bitmap = new EWAHBitmap();
		EWAHBitmap result;

		final int max = 5000001;
		final int halfPos = Math.round((max + 1) / 2) - 1;
		for (int i = 0; i <= halfPos; i++) {
			bitmap.set(i);
		}

		// invert
		result = bitmap.invert(max);
		final int[] resultIds = result.getIds();

		// check the result
		assertEquals(halfPos + 1, resultIds.length);
		for (int i = 0; i <= max; i++) {
			if (i <= halfPos) {
				assertFalse(Arrays.binarySearch(resultIds, i) > -1);
			} else {
				assertTrue(Arrays.binarySearch(resultIds, i) > -1);
			}
		}
	}
}
