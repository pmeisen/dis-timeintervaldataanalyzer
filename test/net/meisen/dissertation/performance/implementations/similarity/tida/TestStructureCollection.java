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
				assertTrue("" + i, i == 0 || i == 1 || i == 5 || i > 7);
			} else {
				assertEquals(0, vals[Relation.UNKNOWN.ordinal() + 1]);
				assertEquals(0, vals[Relation.INVALID.ordinal() + 1]);

				if (i == 2) {
					assertEquals(0, vals[Relation.BEFORE.ordinal() + 1]);
					assertEquals(0, vals[Relation.CONTAINS.ordinal() + 1]);
					assertEquals(0, vals[Relation.COTEMPORAL.ordinal() + 1]);
					assertEquals(0, vals[Relation.FINISHEDBY.ordinal() + 1]);
					assertEquals(0, vals[Relation.MEETS.ordinal() + 1]);
					assertEquals(2, vals[Relation.OVERLAPS.ordinal() + 1]);
					assertEquals(1, vals[Relation.STARTEDBY.ordinal() + 1]);
				} else if (i == 3) {
					assertEquals(0, vals[Relation.BEFORE.ordinal() + 1]);
					assertEquals(2, vals[Relation.CONTAINS.ordinal() + 1]);
					assertEquals(1, vals[Relation.COTEMPORAL.ordinal() + 1]);
					assertEquals(0, vals[Relation.FINISHEDBY.ordinal() + 1]);
					assertEquals(0, vals[Relation.MEETS.ordinal() + 1]);
					assertEquals(2, vals[Relation.OVERLAPS.ordinal() + 1]);
					assertEquals(0, vals[Relation.STARTEDBY.ordinal() + 1]);
				} else if (i == 4) {
					assertEquals(0, vals[Relation.BEFORE.ordinal() + 1]);
					assertEquals(0, vals[Relation.CONTAINS.ordinal() + 1]);
					assertEquals(0, vals[Relation.COTEMPORAL.ordinal() + 1]);
					assertEquals(1, vals[Relation.FINISHEDBY.ordinal() + 1]);
					assertEquals(1, vals[Relation.MEETS.ordinal() + 1]);
					assertEquals(0, vals[Relation.OVERLAPS.ordinal() + 1]);
					assertEquals(0, vals[Relation.STARTEDBY.ordinal() + 1]);
				} else if (i == 6) {
					assertEquals(15, vals[Relation.BEFORE.ordinal() + 1]);
					assertEquals(0, vals[Relation.CONTAINS.ordinal() + 1]);
					assertEquals(3, vals[Relation.COTEMPORAL.ordinal() + 1]);
					assertEquals(0, vals[Relation.FINISHEDBY.ordinal() + 1]);
					assertEquals(0, vals[Relation.MEETS.ordinal() + 1]);
					assertEquals(0, vals[Relation.OVERLAPS.ordinal() + 1]);
					assertEquals(0, vals[Relation.STARTEDBY.ordinal() + 1]);
				} else if (i == 7) {
					assertEquals(5, vals[Relation.BEFORE.ordinal() + 1]);
					assertEquals(0, vals[Relation.CONTAINS.ordinal() + 1]);
					assertEquals(0, vals[Relation.COTEMPORAL.ordinal() + 1]);
					assertEquals(0, vals[Relation.FINISHEDBY.ordinal() + 1]);
					assertEquals(3, vals[Relation.MEETS.ordinal() + 1]);
					assertEquals(0, vals[Relation.OVERLAPS.ordinal() + 1]);
					assertEquals(0, vals[Relation.STARTEDBY.ordinal() + 1]);
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
				assertTrue("" + i, i == 0 || i == 1 || i == 5 || i > 6);
			} else {
				assertEquals(0, vals[Relation.UNKNOWN.ordinal() + 1]);
				assertEquals(0, vals[Relation.INVALID.ordinal() + 1]);

				if (i == 2) {
					assertEquals(0, vals[Relation.BEFORE.ordinal() + 1]);
					assertEquals(0, vals[Relation.CONTAINS.ordinal() + 1]);
					assertEquals(0, vals[Relation.COTEMPORAL.ordinal() + 1]);
					assertEquals(0, vals[Relation.FINISHEDBY.ordinal() + 1]);
					assertEquals(1, vals[Relation.MEETS.ordinal() + 1]);
					assertEquals(2, vals[Relation.OVERLAPS.ordinal() + 1]);
					assertEquals(1, vals[Relation.STARTEDBY.ordinal() + 1]);
				} else if (i == 3) {
					assertEquals(0, vals[Relation.BEFORE.ordinal() + 1]);
					assertEquals(2, vals[Relation.CONTAINS.ordinal() + 1]);
					assertEquals(1, vals[Relation.COTEMPORAL.ordinal() + 1]);
					assertEquals(0, vals[Relation.FINISHEDBY.ordinal() + 1]);
					assertEquals(0, vals[Relation.MEETS.ordinal() + 1]);
					assertEquals(2, vals[Relation.OVERLAPS.ordinal() + 1]);
					assertEquals(0, vals[Relation.STARTEDBY.ordinal() + 1]);
				} else if (i == 4) {
					assertEquals(0, vals[Relation.BEFORE.ordinal() + 1]);
					assertEquals(0, vals[Relation.CONTAINS.ordinal() + 1]);
					assertEquals(0, vals[Relation.COTEMPORAL.ordinal() + 1]);
					assertEquals(1, vals[Relation.FINISHEDBY.ordinal() + 1]);
					assertEquals(0, vals[Relation.MEETS.ordinal() + 1]);
					assertEquals(0, vals[Relation.OVERLAPS.ordinal() + 1]);
					assertEquals(0, vals[Relation.STARTEDBY.ordinal() + 1]);
				} else if (i == 6) {
					assertEquals(0, vals[Relation.BEFORE.ordinal() + 1]);
					assertEquals(0, vals[Relation.CONTAINS.ordinal() + 1]);
					assertEquals(3, vals[Relation.COTEMPORAL.ordinal() + 1]);
					assertEquals(0, vals[Relation.FINISHEDBY.ordinal() + 1]);
					assertEquals(3, vals[Relation.MEETS.ordinal() + 1]);
					assertEquals(0, vals[Relation.OVERLAPS.ordinal() + 1]);
					assertEquals(0, vals[Relation.STARTEDBY.ordinal() + 1]);
				} else {
					fail("Unreachable " + i);
				}
			}
		}
	}

	@Test
	public void test2() {
		final StructureCollection coll = new StructureCollection(
				new DefaultStructureGroupFactory(TouchingStructureGroup.class),
				12);
		Bitmap bmp;

		// @formatter:off
		/*
		 * Create the following:
		 *    012345678901
		 * 0  011110000000
		 * 1  111111000000
		 * 2  000000011110
		 * 3  000000011111
		 */
		// @formatter:on
		bmp = Bitmap.createBitmap(factory, 1);
		coll.set(null, 0, bmp);
		bmp = Bitmap.createBitmap(factory, 0, 1);
		coll.set(null, 1, bmp);
		bmp = Bitmap.createBitmap(factory, 0, 1);
		coll.set(null, 2, bmp);
		bmp = Bitmap.createBitmap(factory, 0, 1);
		coll.set(null, 3, bmp);
		bmp = Bitmap.createBitmap(factory, 0, 1);
		coll.set(null, 4, bmp);
		bmp = Bitmap.createBitmap(factory, 1);
		coll.set(null, 5, bmp);
		coll.set(null, 6, null);
		bmp = Bitmap.createBitmap(factory, 2, 3);
		coll.set(null, 7, bmp);
		bmp = Bitmap.createBitmap(factory, 2, 3);
		coll.set(null, 8, bmp);
		bmp = Bitmap.createBitmap(factory, 2, 3);
		coll.set(null, 9, bmp);
		bmp = Bitmap.createBitmap(factory, 2, 3);
		coll.set(null, 10, bmp);


		// finish the collection
		coll.finish();

		// see the results
		for (int i = 0; i < 100; i++) {
			final int[] vals = coll.get(null, i);

			if (vals != null) {
				System.out.println("-------" + i);
				System.out.println(vals[Relation.BEFORE.ordinal() + 1]);
				System.out.println(vals[Relation.CONTAINS.ordinal() + 1]);
				System.out.println(vals[Relation.COTEMPORAL.ordinal() + 1]);
				System.out.println(vals[Relation.FINISHEDBY.ordinal() + 1]);
				System.out.println(vals[Relation.MEETS.ordinal() + 1]);
				System.out.println(vals[Relation.OVERLAPS.ordinal() + 1]);
				System.out.println(vals[Relation.STARTEDBY.ordinal() + 1]);
			}
		}
	}
}
