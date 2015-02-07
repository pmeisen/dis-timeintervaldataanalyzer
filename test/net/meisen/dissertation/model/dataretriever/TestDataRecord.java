package net.meisen.dissertation.model.dataretriever;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import net.meisen.dissertation.exceptions.DataRetrieverException;
import net.meisen.dissertation.help.ExceptionBasedTest;
import net.meisen.dissertation.model.dataretriever.mock.MockDataCollection;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

import org.junit.Test;
import org.junit.matchers.JUnitMatchers;

/**
 * Tests the implementation of a {@code DataRecord}.
 * 
 * @author pmeisen
 * 
 */
public class TestDataRecord extends ExceptionBasedTest {

	/**
	 * Tests valid constructors.
	 */
	@Test
	public void testValidConstruction() {
		final DataCollection<String> dc = new MockDataCollection<String>(
				new String[] { "A", "B", "C" });

		assertNotNull(new DataRecord<String>(dc));
		assertNotNull(new DataRecord<String>(dc, null));
		assertNotNull(new DataRecord<String>(dc, new Object[] { 5, "A" }));
		assertNotNull(new DataRecord<String>(dc, new Integer[] { 4 }));
		assertNotNull(new DataRecord<String>(dc, new String[] {}));
		assertNotNull(new DataRecord<String>(dc, new Integer[] {}));
		assertNotNull(new DataRecord<String>(dc, new Integer[] { 2, 3, 4 }));
		assertNotNull(new DataRecord<String>(dc, null));
	}

	/**
	 * Tests the creation of an empty record.
	 */
	@Test
	public void testEmptyDataRecord() {
		final DataCollection<Object> dc = new MockDataCollection<Object>(
				new Object[] {});
		final DataRecord<Object> rec = new DataRecord<Object>(dc);

		// check some other sample values
		assertNotNull(rec.getData());
		assertEquals(0, rec.getSize());

		// check if an error occurs when retrieving invalid position
		boolean error = false;
		try {
			rec.getDataByPos(0);
		} catch (final IndexOutOfBoundsException e) {
			error = true;
		}
		assertTrue(error);
	}

	/**
	 * Tests the implementation of {@code DataRecord#getDataByPos(int)}.
	 */
	@Test
	public void testGetDataByPos() {
		final DataCollection<Integer> dc = new MockDataCollection<Integer>(
				new Integer[] { 5, 6, 7 });
		final DataRecord<Integer> rec = new DataRecord<Integer>(dc,
				new String[] { "A", "B", "C" });

		assertEquals(3, rec.getSize());
		assertEquals("A", rec.getDataByPos(0));
		assertEquals("B", rec.getDataByPos(1));
		assertEquals("C", rec.getDataByPos(2));
		assertEquals("A", rec.getData((Integer) 5));
	}

	/**
	 * Tests the implementation of {@code DataRecord#getNameOfPos(int)}.
	 */
	@Test
	public void testGetNameOfPos() {
		final DataCollection<String> dc = new MockDataCollection<String>(
				new String[] { "A", "B", "C" });
		final DataRecord<String> rec = new DataRecord<String>(dc, new String[] {
				"valA", "valB", "valC" });

		rec.getNames();
		assertEquals(3, rec.getSize());
		assertEquals("A", rec.getNameOfPos(0));
		assertEquals("B", rec.getNameOfPos(1));
		assertEquals("C", rec.getNameOfPos(2));
	}

	/**
	 * Tests some invalid length usage
	 */
	@Test
	public void testExceptionInvalidLengths() {
		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(DataRetrieverException.class,
				1004));

		final DataCollection<String> dc = new MockDataCollection<String>(
				new String[] { "A", "B", "C" });
		new DataRecord<String>(dc, new Integer[] { 5, 6, 7, 8 });
	}

	/**
	 * Tests the usage of an invalid position.
	 */
	@Test
	public void testExceptionInvalidGetPosition() {
		thrown.expect(IndexOutOfBoundsException.class);
		thrown.expectMessage(JUnitMatchers.containsString("Index: 5, Size: 3"));

		final DataCollection<Integer> dc = new MockDataCollection<Integer>(
				new Integer[] { 5, 6, 7 });
		final DataRecord<Integer> rec = new DataRecord<Integer>(dc,
				new String[] { "A", "B", "C" });
		rec.getDataByPos(5);
	}
}
