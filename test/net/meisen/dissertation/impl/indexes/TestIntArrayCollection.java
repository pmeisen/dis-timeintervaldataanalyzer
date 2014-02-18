package net.meisen.dissertation.impl.indexes;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import net.meisen.dissertation.impl.indexes.mock.IntegerBasedValue;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;

import org.junit.Before;
import org.junit.Test;

/**
 * Tests the implementation of a {@code ArrayCollection}
 * 
 * @author pmeisen
 * 
 */
public class TestIntArrayCollection {
	private IntArrayCollection coll;

	/**
	 * Creates a {@code ArrayCollection} which is tested.
	 */
	@Before
	public void setup() {
		final IndexKeyDefinition key = new IndexKeyDefinition(
				IntegerBasedValue.class, "getId");

		coll = new IntArrayCollection(key);
		IntegerBasedValue.reset();
	}

	/**
	 * Tests the creation
	 */
	@Test
	public void testGetAndAdd() {
		final int maxValue = 10000;

		// check that there aren't any values available
		assertNull(coll.getObject(-1));
		assertNull(coll.getObject(maxValue));
		for (int i = 0; i <= maxValue; i++) {
			assertNull(coll.getObject(i));
		}

		// set a new size and check again
		coll.setMaxValue(maxValue);
		assertNull(coll.getObject(-1));
		assertNull(coll.getObject(maxValue));
		for (int i = 0; i <= maxValue; i++) {
			assertNull(coll.getObject(i));
		}

		// add some values and check that those are available
		for (int i = 0; i <= maxValue; i++) {
			final IntegerBasedValue value = new IntegerBasedValue();

			// add the value
			coll.addObject(value);

			// check if the size is increased
			assertEquals(i + 1, coll.size());

			// check if the value was added correctly
			assertEquals(value, coll.getObject(value.getId()));
		}

		// test min and max
		assertEquals(0, coll.getMinValue());
		assertEquals(maxValue, coll.getMaxValue());
	}

	/**
	 * Tests the down-scaling.
	 */
	@Test
	public void testDownScaling() {
		final int initMaxValue = 10;

		// add 10 values
		coll.setMaxValue(initMaxValue);
		for (int i = 0; i <= initMaxValue; i++) {
			coll.addObject(new IntegerBasedValue());
		}
		for (int i = 0; i <= initMaxValue; i++) {
			assertEquals(i, ((IntegerBasedValue) coll.getObject(i)).getId());
		}

		// test min and max with 10
		assertEquals(0, coll.getMinValue());
		assertEquals(initMaxValue, coll.getMaxValue());

		// change size to 5
		coll.setMaxValue(5);
		for (int i = 0; i <= initMaxValue; i++) {
			if (i <= 5) {
				assertEquals(i, ((IntegerBasedValue) coll.getObject(i)).getId());
			} else {
				assertNull(coll.getObject(i));
			}
		}

		// test min and max with 5
		assertEquals(0, coll.getMinValue());
		assertEquals(5, coll.getMaxValue());

		// add invalid value
		coll.addObject(new IntegerBasedValue());
	}

	/**
	 * Tests the up-scaling.
	 */
	@Test
	public void testUpScaling() {
		final int initMaxValue = 20;

		// add 20 values
		coll.setMaxValue(initMaxValue);
		for (int i = 0; i <= initMaxValue; i++) {
			coll.addObject(new IntegerBasedValue());
		}
		for (int i = 0; i <= initMaxValue; i++) {
			assertEquals(i, ((IntegerBasedValue) coll.getObject(i)).getId());
		}

		// test min and max with 20
		assertEquals(0, coll.getMinValue());
		assertEquals(initMaxValue, coll.getMaxValue());

		// change size to 25
		coll.setMaxValue(25);
		for (int i = 0; i <= 25; i++) {
			if (i <= initMaxValue) {
				assertEquals(i, ((IntegerBasedValue) coll.getObject(i)).getId());
			} else {
				assertEquals(25, coll.getMaxValue());
				assertNull(coll.getObject(i));
			}
		}
	}

	/**
	 * Tests overwrite.
	 */
	@Test
	public void testOverwrite() {
		final int initAmount = 10;

		coll.setMaxValue(initAmount);
		for (int i = 0; i <= initAmount; i++) {
			final IntegerBasedValue val = new IntegerBasedValue();
			assertTrue(coll.addObject(val));
			assertFalse(coll.addObject(val));
		}

		// get the current object
		final Object valAtInitAmount = coll.getObject(initAmount);

		// remove the object
		coll.removeObject(valAtInitAmount);

		// add a new value to the position
		IntegerBasedValue.reset(10);
		assertTrue(coll.addObject(new IntegerBasedValue()));
		for (int i = 0; i <= initAmount; i++) {
			assertNotNull(coll.getObject(i));
		}
	}

	/**
	 * Tests the implementation of {@link IntArrayCollection#getAll()}.
	 */
	@Test
	public void testGetAll() {
		final int initAmount = 100;
		coll.setMaxValue(initAmount);

		assertEquals(0, coll.getAll().size());
	}

	/**
	 * Tests the implementation of
	 * {@link IntArrayCollection#getObjectsByRange(Object, long)} and
	 * {@link IntArrayCollection#getObjectsByStartAndEnd(Object, Object)}.
	 */
	@Test
	public void testGetObjects() {
		final int initAmount = 200;
		coll.setMaxValue(initAmount);

		// adds some objects
		for (int i = 0; i <= initAmount; i++) {
			final IntegerBasedValue val = new IntegerBasedValue();
			assertTrue(coll.addObject(val));
		}

		Object[] objects;
		int offset;
		int i;

		// 20 - 30
		offset = 20;
		objects = coll.getObjectsByStartAndEnd(offset, 30);
		for (i = 0; i < objects.length; i++) {
			assertEquals(i + offset, ((IntegerBasedValue) objects[i]).getId());
		}
		assertEquals(31 - offset, i);

		// 0 - 2000
		offset = 0;
		objects = coll.getObjectsByStartAndEnd(0, 2000);
		for (i = 0; i < objects.length; i++) {
			assertEquals(i + offset, ((IntegerBasedValue) objects[i]).getId());
		}
		assertEquals(201 - offset, i);

		// 0 - 0
		offset = 0;
		objects = coll.getObjectsByStartAndEnd(offset, 0);
		assertEquals(1, objects.length);

		// 0 - 0 (amount)
		offset = 0;
		objects = coll.getObjectsByRange(offset, 0);
		assertEquals(0, objects.length);

		// 100 - 0 (amount)
		offset = 100;
		objects = coll.getObjectsByRange(offset, 0);
		assertEquals(0, objects.length);

		// 100 - 1 (amount)
		offset = 100;
		objects = coll.getObjectsByRange(offset, 1);
		assertEquals(1, objects.length);
		assertEquals(100, ((IntegerBasedValue) objects[0]).getId());

		// 100 - 200 (amount)
		offset = 100;
		objects = coll.getObjectsByRange(offset, 200);
		assertEquals(101, objects.length);
		for (i = 0; i < objects.length; i++) {
			assertEquals(i + offset, ((IntegerBasedValue) objects[i]).getId());
		}
		assertEquals(201 - offset, i);
	}

	/**
	 * Tests the implementation of
	 * {@link IntArrayCollection#iterateByRange(Object, long)} and
	 * {@link IntArrayCollection#iterateByStartAndEnd(Object, Object)}.
	 */
	@Test
	public void testIteration() {
		final int initAmount = 200;
		coll.setMaxValue(initAmount);

		// adds some objects
		for (int i = 0; i <= initAmount; i++) {
			final IntegerBasedValue val = new IntegerBasedValue();
			assertTrue(coll.addObject(val));
		}

		// get some iterator
		IntArrayIterator it;
		int i;

		// test iterator 20 - 30
		it = coll.iterateByStartAndEnd(20, 30);
		i = 20;
		while (it.hasNext()) {
			assertTrue(it.hasNext());
			assertEquals(i, ((IntegerBasedValue) it.next()).getId());
			i++;
		}
		assertEquals(31, i);

		// test iterator min - max
		it = coll.iterateByStartAndEnd(coll.getMinValue(), coll.getMaxValue());
		i = 0;
		while (it.hasNext()) {
			assertEquals(i, ((IntegerBasedValue) it.next()).getId());
			i++;
		}
		assertEquals(201, i);

		// test 0 iterator
		it = coll.iterateByStartAndEnd(0, 0);
		assertTrue(it.hasNext());
		assertEquals(0, ((IntegerBasedValue) it.next()).getId());
		assertFalse(it.hasNext());

		// test 0 - 1 iterator
		it = coll.iterateByRange(0, 0);
		assertFalse(it.hasNext());

		// test to much 0 - 2000
		it = coll.iterateByStartAndEnd(0, 2000);
		i = 0;
		while (it.hasNext()) {
			assertEquals(i, ((IntegerBasedValue) it.next()).getId());
			i++;
		}
		assertEquals(201, i);

		// test invalid iterators
		it = coll.iterateByStartAndEnd(-1, 0);
		assertFalse(it.hasNext());
		it = coll.iterateByStartAndEnd(-1, -100);
		assertFalse(it.hasNext());
		it = coll.iterateByStartAndEnd(1, -100);
		assertFalse(it.hasNext());
	}
}
