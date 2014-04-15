package net.meisen.dissertation.impl.indexes.datarecord.intervalindex;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import net.meisen.dissertation.impl.indexes.IndexFactory;
import net.meisen.dissertation.impl.time.mapper.LongMapper;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry.IntervalTypeFactory.IntervalType;
import net.meisen.dissertation.model.indexes.datarecord.slices.IndexDimensionSlice;
import net.meisen.dissertation.model.time.granularity.Day;
import net.meisen.general.genmisc.types.Numbers;

import org.junit.Before;
import org.junit.Test;

/**
 * Tests the implementation of the {@code ByteIntervalIndex}.
 * 
 * @author pmeisen
 * 
 */
public class TestByteIntervalIndex {
	private ByteIntervalIndex byteIdx;

	/**
	 * Initializes the index with some values.
	 */
	@Before
	public void init() {

		// let's create an index
		final IndexFactory idxFactory = new IndexFactory();
		final LongMapper mapper = new LongMapper(1000, 1100, Day.instance());
		byteIdx = new ByteIntervalIndex(mapper, new IntervalStructureEntry(
				IntervalType.START, "START"), new IntervalStructureEntry(
				IntervalType.END, "END"), idxFactory);

		// let's add data
		for (int i = 0; i < 50; i++) {
			byteIdx.index(i, i + 1000, i + 1010);
		}
	}

	/**
	 * Tests the inclusion of values using an interval {@code [a, b]}.
	 */
	@Test
	public void testInclusionAllSlicesAvailable() {
		final IndexDimensionSlice<?>[] res = byteIdx
				.getSlices(1001, 1050, true, true);

		assertEquals(50, res.length);
		for (int i = 0; i < 50; i++) {
			assertEquals(Numbers.castToByte(i + 1), res[i].getId());
		}
	}

	/**
	 * Tests the inclusion of values using an interval {@code [a, b]}, whereby
	 * not all slices are available.
	 */
	@Test
	public void testInclusionSlicesPartiallyAvailable() {
		final IndexDimensionSlice<?>[] res = byteIdx
				.getSlices(1001, 1100, true, true);

		/*
		 * We expect to retrieve values for the first 1001 - 1059 (1010 + 49)
		 * entries.
		 */
		assertEquals(100, res.length);
		for (int i = 0; i < 100; i++) {
			if (i < 59) {
				assertEquals(Numbers.castToByte(i + 1), res[i].getId());
			} else {
				assertNull("Not null: " + i, res[i]);
			}
		}
	}

	/**
	 * Tests the exclusion of values using an interval {@code (a, b)}.
	 */
	@Test
	public void testExclusionAllSlicesAvailable() {
		final IndexDimensionSlice<?>[] res = byteIdx
				.getSlices(1001, 1050, false, false);

		assertEquals(48, res.length);
		for (int i = 0; i < 48; i++) {
			assertEquals(Numbers.castToByte(i + 2), res[i].getId());
		}
	}

	/**
	 * Tests the exclusion of values using an interval {@code (a, b]} or
	 * {@code [a, b)}.
	 */
	@Test
	public void testPartialAllSlicesAvailable() {
		final IndexDimensionSlice<?>[] resLeft = byteIdx
				.getSlices(1021, 1050, true, false);

		assertEquals(29, resLeft.length);
		for (int i = 0; i < 29; i++) {
			assertEquals(Numbers.castToByte(i + 21), resLeft[i].getId());
		}

		final IndexDimensionSlice<?>[] resRight = byteIdx
				.getSlices(1021, 1050, false, true);

		assertEquals(29, resRight.length);
		for (int i = 0; i < 29; i++) {
			assertEquals(Numbers.castToByte(i + 22), resRight[i].getId());
		}
	}

	/**
	 * Tests the data retrieval for invalid values, i.e. {@code [1000, 500]}.
	 */
	@Test
	public void testInclusionInvalidInterval() {
		final IndexDimensionSlice<?>[] res = byteIdx
				.getSlices(1051, 1050, true, true);

		assertEquals(0, res.length);
	}

	/**
	 * Tests the data retrieval for invalid intervals, i.e. {@code (1050, 1050]}
	 * .
	 */
	@Test
	public void testExclusionInvalidInterval() {
		final IndexDimensionSlice<?>[] res = byteIdx
				.getSlices(1050, 1050, false, true);
		assertEquals(0, res.length);
	}
}
