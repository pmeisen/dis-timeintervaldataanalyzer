package net.meisen.dissertation.performance.implementations.similarity.tida;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import net.meisen.dissertation.impl.indexes.IndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.performance.implementations.similarity.tida.TemporalRelation.Relation;

import org.junit.Test;

/**
 * Tests the implementation of the {@code StructureCollection}.
 * 
 * @author pmeisen
 * 
 */
public class TestStructureCollection {
	private static final IndexFactory factory = new IndexFactory();

	/**
	 * Tests the {@code StructureCollection} considering counting using a
	 * {@code AllStructureGroup} and:
	 * 
	 * <pre>
	 * 0110000
	 * 1100000
	 * 0011000
	 * 0010000
	 * 1111000
	 * 0000011
	 * </pre>
	 */
	@Test
	public void testCountingWithAllStructureGroup() {
		final StructureCollection coll = new StructureCollection(
				new DefaultStructureGroupFactory(AllStructureGroup.class), 7);
		Bitmap bmp;

		// @formatter:off
		/*
		 * Create the following:
		 * 0  0110000
		 * 1  1100000
		 * 2  0011000
		 * 3  0110000
		 * 4  1111000
		 * 5  0000010
		 * 6  0000010
		 * 7  0000010
		 * 8  0000001
		 */
		// @formatter:on
		bmp = Bitmap.createBitmap(factory, 1, 4);
		coll.set(null, 0, bmp);
		bmp = Bitmap.createBitmap(factory, 0, 1, 3, 4);
		coll.set(null, 1, bmp);
		bmp = Bitmap.createBitmap(factory, 0, 2, 3, 4);
		coll.set(null, 2, bmp);
		bmp = Bitmap.createBitmap(factory, 2, 4);
		coll.set(null, 3, bmp);
		// add a null line 0...0
		coll.set(null, 4, null);
		bmp = Bitmap.createBitmap(factory, 5, 6, 7);
		coll.set(null, 5, bmp);
		bmp = Bitmap.createBitmap(factory, 8);
		coll.set(null, 6, bmp);

		// finish the collection
		coll.finish();

		// see the results
		for (int i = 0; i < 100; i++) {
			final int[] vals = coll.get(null, i);

			if (vals == null) {
				assertTrue("" + i, i == 0 || i == 4 || i > 6);
			} else {
				assertEquals(0, vals[Relation.UNKNOWN.ordinal()]);
				assertEquals(0, vals[Relation.INVALID.ordinal()]);

				if (i == 1) {
					assertEquals(0, vals[Relation.BEFORE.ordinal()]);
					assertEquals(0, vals[Relation.CONTAINS.ordinal()]);
					assertEquals(0, vals[Relation.COTEMPORAL.ordinal()]);
					assertEquals(0, vals[Relation.FINISHEDBY.ordinal()]);
					assertEquals(0, vals[Relation.MEETS.ordinal()]);
					assertEquals(2, vals[Relation.OVERLAPS.ordinal()]);
					assertEquals(1, vals[Relation.STARTEDBY.ordinal()]);
				} else if (i == 2) {
					assertEquals(0, vals[Relation.BEFORE.ordinal()]);
					assertEquals(2, vals[Relation.CONTAINS.ordinal()]);
					assertEquals(1, vals[Relation.COTEMPORAL.ordinal()]);
					assertEquals(0, vals[Relation.FINISHEDBY.ordinal()]);
					assertEquals(0, vals[Relation.MEETS.ordinal()]);
					assertEquals(2, vals[Relation.OVERLAPS.ordinal()]);
					assertEquals(0, vals[Relation.STARTEDBY.ordinal()]);
				} else if (i == 3) {
					assertEquals(0, vals[Relation.BEFORE.ordinal()]);
					assertEquals(0, vals[Relation.CONTAINS.ordinal()]);
					assertEquals(0, vals[Relation.COTEMPORAL.ordinal()]);
					assertEquals(1, vals[Relation.FINISHEDBY.ordinal()]);
					assertEquals(1, vals[Relation.MEETS.ordinal()]);
					assertEquals(0, vals[Relation.OVERLAPS.ordinal()]);
					assertEquals(0, vals[Relation.STARTEDBY.ordinal()]);
				} else if (i == 5) {
					assertEquals(15, vals[Relation.BEFORE.ordinal()]);
					assertEquals(0, vals[Relation.CONTAINS.ordinal()]);
					assertEquals(3, vals[Relation.COTEMPORAL.ordinal()]);
					assertEquals(0, vals[Relation.FINISHEDBY.ordinal()]);
					assertEquals(0, vals[Relation.MEETS.ordinal()]);
					assertEquals(0, vals[Relation.OVERLAPS.ordinal()]);
					assertEquals(0, vals[Relation.STARTEDBY.ordinal()]);
				} else if (i == 6) {
					assertEquals(5, vals[Relation.BEFORE.ordinal()]);
					assertEquals(0, vals[Relation.CONTAINS.ordinal()]);
					assertEquals(0, vals[Relation.COTEMPORAL.ordinal()]);
					assertEquals(0, vals[Relation.FINISHEDBY.ordinal()]);
					assertEquals(3, vals[Relation.MEETS.ordinal()]);
					assertEquals(0, vals[Relation.OVERLAPS.ordinal()]);
					assertEquals(0, vals[Relation.STARTEDBY.ordinal()]);
				} else {
					fail("Unreachable " + i);
				}
			}
		}
	}

	/**
	 * Tests the {@code StructureCollection} considering counting using a
	 * {@code TouchingStructureGroup} and:
	 * 
	 * <pre>
	 * 0110000
	 * 1100000
	 * 0011000
	 * 0010000
	 * 1111000
	 * 0000011
	 * </pre>
	 */
	@Test
	public void testCountingWithTouchingStructureGroup() {
		final StructureCollection coll = new StructureCollection(
				new DefaultStructureGroupFactory(TouchingStructureGroup.class),
				7);
		Bitmap bmp;

		// @formatter:off
		/*
		 * Create the following:
		 * 0  0110000
		 * 1  1100000
		 * 2  0011000
		 * 3  0110000
		 * 4  1111000
		 * 5  0000010
		 * 6  0000010
		 * 7  0000010
		 * 8  0000001
		 */
		// @formatter:on
		bmp = Bitmap.createBitmap(factory, 1, 4);
		coll.set(null, 0, bmp);
		bmp = Bitmap.createBitmap(factory, 0, 1, 3, 4);
		coll.set(null, 1, bmp);
		bmp = Bitmap.createBitmap(factory, 0, 2, 3, 4);
		coll.set(null, 2, bmp);
		bmp = Bitmap.createBitmap(factory, 2, 4);
		coll.set(null, 3, bmp);
		// add a null line 0...0
		coll.set(null, 4, null);
		bmp = Bitmap.createBitmap(factory, 5, 6, 7);
		coll.set(null, 5, bmp);
		bmp = Bitmap.createBitmap(factory, 8);
		coll.set(null, 6, bmp);

		// finish the collection
		coll.finish();

		// see the results
		for (int i = 0; i < 100; i++) {
			final int[] vals = coll.get(null, i);

			if (vals == null) {
				assertTrue("" + i, i == 0 || i == 4 || i > 5);
			} else {
				assertEquals(0, vals[Relation.UNKNOWN.ordinal()]);
				assertEquals(0, vals[Relation.INVALID.ordinal()]);

				if (i == 1) {
					assertEquals(0, vals[Relation.BEFORE.ordinal()]);
					assertEquals(0, vals[Relation.CONTAINS.ordinal()]);
					assertEquals(0, vals[Relation.COTEMPORAL.ordinal()]);
					assertEquals(0, vals[Relation.FINISHEDBY.ordinal()]);
					assertEquals(1, vals[Relation.MEETS.ordinal()]);
					assertEquals(2, vals[Relation.OVERLAPS.ordinal()]);
					assertEquals(1, vals[Relation.STARTEDBY.ordinal()]);
				} else if (i == 2) {
					assertEquals(0, vals[Relation.BEFORE.ordinal()]);
					assertEquals(2, vals[Relation.CONTAINS.ordinal()]);
					assertEquals(1, vals[Relation.COTEMPORAL.ordinal()]);
					assertEquals(0, vals[Relation.FINISHEDBY.ordinal()]);
					assertEquals(0, vals[Relation.MEETS.ordinal()]);
					assertEquals(2, vals[Relation.OVERLAPS.ordinal()]);
					assertEquals(0, vals[Relation.STARTEDBY.ordinal()]);
				} else if (i == 3) {
					assertEquals(0, vals[Relation.BEFORE.ordinal()]);
					assertEquals(0, vals[Relation.CONTAINS.ordinal()]);
					assertEquals(0, vals[Relation.COTEMPORAL.ordinal()]);
					assertEquals(1, vals[Relation.FINISHEDBY.ordinal()]);
					assertEquals(0, vals[Relation.MEETS.ordinal()]);
					assertEquals(0, vals[Relation.OVERLAPS.ordinal()]);
					assertEquals(0, vals[Relation.STARTEDBY.ordinal()]);
				} else if (i == 5) {
					assertEquals(0, vals[Relation.BEFORE.ordinal()]);
					assertEquals(0, vals[Relation.CONTAINS.ordinal()]);
					assertEquals(3, vals[Relation.COTEMPORAL.ordinal()]);
					assertEquals(0, vals[Relation.FINISHEDBY.ordinal()]);
					assertEquals(3, vals[Relation.MEETS.ordinal()]);
					assertEquals(0, vals[Relation.OVERLAPS.ordinal()]);
					assertEquals(0, vals[Relation.STARTEDBY.ordinal()]);
				} else {
					fail("Unreachable " + i);
				}
			}
		}
	}
}
