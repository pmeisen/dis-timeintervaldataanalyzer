package net.meisen.dissertation.impl.indexes.datarecord.bitmap;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.junit.Test;

public class TestEWAHBitmap {

	@Test
	public void testNegativePosition() {
		final EWAHBitmap bitmap = new EWAHBitmap();
		EWAHBitmap result;

		// get an invalid position
		result = bitmap.invert(-1);
		assertEquals(0, result.getIds().length);
	}

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

	@Test
	public void testSimpleInvert() {
		final EWAHBitmap bitmap = new EWAHBitmap();
		EWAHBitmap result;

		// set the first value
		bitmap.set(1, 5, 10);

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
		assertTrue(Arrays.binarySearch(result.getIds(), 9) > -1);
	}
}
