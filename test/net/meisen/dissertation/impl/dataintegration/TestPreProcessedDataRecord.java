package net.meisen.dissertation.impl.dataintegration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.LinkedHashMap;
import java.util.Map;

import net.meisen.dissertation.help.ExceptionBasedTest;
import net.meisen.dissertation.impl.datasets.SingleStaticDataSet;

import org.junit.Test;

/**
 * Tests the implementation of a {@code PreProcessedDataRecord}.
 * 
 * @author pmeisen
 * 
 */
public class TestPreProcessedDataRecord extends ExceptionBasedTest {

	/**
	 * Tests a simple wrapping.
	 */
	@Test
	public void testSimpleWrapping() {
		final Map<String, Object> map = new LinkedHashMap<String, Object>();
		map.put("NAME", 5);

		// create a wrapper around the instance
		final PreProcessedDataRecord rec = new PreProcessedDataRecord(
				new SingleStaticDataSet(map));

		assertEquals(5, rec.getValue(1));
		assertEquals(5, rec.getValue("NAME"));

		assertTrue(rec.isValidPosition(1));
		assertFalse(rec.isValidPosition(0));
		assertFalse(rec.isValidPosition(2));

		assertTrue(rec.hasNamedValue("NAME"));
		assertFalse(rec.hasNamedValue("AnotherName"));

		assertEquals("NAME", rec.getName(1));
		assertEquals(1, rec.getPosition("NAME"));
		assertTrue(rec.getPosition("AnotherName") < 1);

	}

	/**
	 * Tests the overriding of valuse.
	 */
	@Test
	public void testOverrideValues() {
		final Map<String, Object> map = new LinkedHashMap<String, Object>();
		map.put("NAME1", 5);
		map.put("NAME2", 10.5);
		map.put("NAME3", "STRING");
		final SingleStaticDataSet oldRec = new SingleStaticDataSet(map);

		final PreProcessedDataRecord rec = new PreProcessedDataRecord(oldRec);

		// set the value
		rec.setValue("NAME3", "NEWVALUE");
		rec.setValue(2, 12.0);

		// check
		assertEquals(5, rec.getValue(1));
		assertEquals(5, rec.getValue("NAME1"));
		assertEquals(12.0, rec.getValue(2));
		assertEquals(12.0, rec.getValue("NAME2"));
		assertEquals("NEWVALUE", rec.getValue(3));
		assertEquals("NEWVALUE", rec.getValue("NAME3"));

		assertTrue(rec.isValidPosition(1));
		assertTrue(rec.isValidPosition(2));
		assertTrue(rec.isValidPosition(3));
		assertFalse(rec.isValidPosition(0));
		assertFalse(rec.isValidPosition(4));

		assertTrue(rec.hasNamedValue("NAME1"));
		assertTrue(rec.hasNamedValue("NAME2"));
		assertTrue(rec.hasNamedValue("NAME3"));
		assertFalse(rec.hasNamedValue("NAME"));

		assertEquals("NAME1", rec.getName(1));
		assertEquals("NAME2", rec.getName(2));
		assertEquals("NAME3", rec.getName(3));
		assertEquals(1, rec.getPosition("NAME1"));
		assertEquals(2, rec.getPosition("NAME2"));
		assertEquals(3, rec.getPosition("NAME3"));
		assertTrue(rec.getPosition("NAME") < 1);

		// set another value
		rec.setValue("NAME3", "NEWVALUE");
		rec.setValue(2, 13.6);

		// check
		assertEquals(5, rec.getValue(1));
		assertEquals(5, rec.getValue("NAME1"));
		assertEquals(13.6, rec.getValue(2));
		assertEquals(13.6, rec.getValue("NAME2"));
		assertEquals("NEWVALUE", rec.getValue(3));
		assertEquals("NEWVALUE", rec.getValue("NAME3"));

		assertTrue(rec.isValidPosition(1));
		assertTrue(rec.isValidPosition(2));
		assertTrue(rec.isValidPosition(3));
		assertFalse(rec.isValidPosition(0));
		assertFalse(rec.isValidPosition(4));

		assertTrue(rec.hasNamedValue("NAME1"));
		assertTrue(rec.hasNamedValue("NAME2"));
		assertTrue(rec.hasNamedValue("NAME3"));
		assertFalse(rec.hasNamedValue("NAME"));

		assertEquals("NAME1", rec.getName(1));
		assertEquals("NAME2", rec.getName(2));
		assertEquals("NAME3", rec.getName(3));
		assertEquals(1, rec.getPosition("NAME1"));
		assertEquals(2, rec.getPosition("NAME2"));
		assertEquals(3, rec.getPosition("NAME3"));
		assertTrue(rec.getPosition("NAME") < 1);
	}

	/**
	 * Tests the adding of new values.
	 */
	@Test
	public void testAddingValues() {
		final Map<String, Object> map = new LinkedHashMap<String, Object>();
		map.put("NAME1", 5);
		map.put("NAME2", 10.5);
		map.put("NAME3", "STRING");
		final SingleStaticDataSet oldRec = new SingleStaticDataSet(map);

		final PreProcessedDataRecord rec = new PreProcessedDataRecord(oldRec);

		rec.setValue("anewname", "NEW");
		rec.setValue(10, "NEW10");
		rec.setValue(8, "anotherOne", "another");

		// check
		assertEquals(5, rec.getValue(1));
		assertEquals(5, rec.getValue("NAME1"));
		assertEquals(10.5, rec.getValue(2));
		assertEquals(10.5, rec.getValue("NAME2"));
		assertEquals("STRING", rec.getValue(3));
		assertEquals("STRING", rec.getValue("NAME3"));
		assertEquals("NEW", rec.getValue(4));
		assertEquals("NEW", rec.getValue("anewname"));
		assertEquals("NEW10", rec.getValue(10));
		assertEquals("another", rec.getValue("anotherOne"));
		assertEquals("another", rec.getValue(8));

		assertTrue(rec.isValidPosition(1));
		assertTrue(rec.isValidPosition(2));
		assertTrue(rec.isValidPosition(3));
		assertTrue(rec.isValidPosition(4));
		assertTrue(rec.isValidPosition(8));
		assertTrue(rec.isValidPosition(10));
		assertFalse(rec.isValidPosition(0));
		assertFalse(rec.isValidPosition(5));
		assertFalse(rec.isValidPosition(6));
		assertFalse(rec.isValidPosition(7));
		assertFalse(rec.isValidPosition(9));
		assertFalse(rec.isValidPosition(11));

		assertTrue(rec.hasNamedValue("NAME1"));
		assertTrue(rec.hasNamedValue("NAME2"));
		assertTrue(rec.hasNamedValue("NAME3"));
		assertTrue(rec.hasNamedValue("anotherOne"));
		assertTrue(rec.hasNamedValue("anewname"));

		assertEquals("NAME1", rec.getName(1));
		assertEquals("NAME2", rec.getName(2));
		assertEquals("NAME3", rec.getName(3));
		assertEquals("anewname", rec.getName(4));
		assertEquals("anotherOne", rec.getName(8));
		assertNotNull(rec.getName(10));
		assertEquals(1, rec.getPosition("NAME1"));
		assertEquals(2, rec.getPosition("NAME2"));
		assertEquals(3, rec.getPosition("NAME3"));
		assertEquals(4, rec.getPosition("anewname"));
		assertEquals(8, rec.getPosition("anotherOne"));
		assertEquals(10, rec.getPosition(rec.getName(10)));
		assertTrue(rec.getPosition("NAME") < 1);
	}

	/**
	 * Tests the exception to be thrown if no position and name is provided when
	 * calling {@link PreProcessedDataRecord#setValue(int, String, Object)}.
	 */
	@Test
	public void testInvalidValues() {
		final Map<String, Object> map = new LinkedHashMap<String, Object>();
		map.put("NAME1", 5);
		map.put("NAME2", 10.5);
		map.put("NAME3", "STRING");
		final SingleStaticDataSet oldRec = new SingleStaticDataSet(map);

		final PreProcessedDataRecord rec = new PreProcessedDataRecord(oldRec);

		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage("One of the values position or name must be specified");
		rec.setValue(-1, null, "VALUE");
	}

	/**
	 * Tests the exception to be thrown if an invalid pair of position and name
	 * is provided when calling
	 * {@link PreProcessedDataRecord#setValue(int, String, Object)}.
	 */
	@Test
	public void testInvalidValueAssociation() {
		final Map<String, Object> map = new LinkedHashMap<String, Object>();
		map.put("NAME1", 5);
		map.put("NAME2", 10.5);
		map.put("NAME3", "STRING");
		final SingleStaticDataSet oldRec = new SingleStaticDataSet(map);

		final PreProcessedDataRecord rec = new PreProcessedDataRecord(oldRec);

		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage("does not fit the name");
		rec.setValue(1, "NAME3", null);
	}

	/**
	 * Tests the exception to be thrown if an invalid pair of position and name
	 * is provided when calling
	 * {@link PreProcessedDataRecord#setValue(int, String, Object)}.
	 */
	@Test
	public void testInvalidValueAssociationAfterAdding() {
		final Map<String, Object> map = new LinkedHashMap<String, Object>();
		map.put("NAME1", 5);
		map.put("NAME2", 10.5);
		map.put("NAME3", "STRING");
		final SingleStaticDataSet oldRec = new SingleStaticDataSet(map);

		final PreProcessedDataRecord rec = new PreProcessedDataRecord(oldRec);

		rec.setValue("NAME4", "VALUE");

		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage("does not fit the name");
		rec.setValue(5, "NAME4", null);
	}
}
