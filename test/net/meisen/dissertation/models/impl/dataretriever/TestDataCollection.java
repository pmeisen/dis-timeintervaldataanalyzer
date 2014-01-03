package net.meisen.dissertation.models.impl.dataretriever;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Collection;

import net.meisen.dissertation.models.impl.dataretriever.mock.MockDataCollection;

import org.junit.Test;

/**
 * Tests the implementation of a {@code DataCollection}.
 * 
 * @author pmeisen
 * 
 */
public class TestDataCollection {

	/**
	 * Tests an empty {@code DataCollection}.
	 */
	@Test
	public void testEmpty() {
		boolean error = false;

		// create an empty Mock
		final DataCollection<String> dc = new MockDataCollection<String>(
				new String[] {});

		// check the values
		assertEquals(0, dc.getRecordSize());
		assertEquals(-1, dc.getPosOfName("unknown"));
		assertEquals(0, dc.getNames().size());

		// check if an error occurs when retrieving invalid position
		error = false;
		try {
			dc.getNameOfPos(0);
		} catch (final IndexOutOfBoundsException e) {
			error = true;
		}
		assertTrue(error);
	}

	/**
	 * Test a {@code DataCollection} with names.
	 */
	@Test
	public void testWithNames() {

		// create a mock with two names
		final DataCollection<String> dc = new MockDataCollection<String>(
				new String[] { "FirstName", "SecondName" });

		assertEquals(2, dc.getRecordSize());
		assertEquals(-1, dc.getPosOfName("unknown"));
		assertEquals(0, dc.getPosOfName("FirstName"));
		assertEquals(1, dc.getPosOfName("SecondName"));
		assertEquals("FirstName", dc.getNameOfPos(0));
		assertEquals("SecondName", dc.getNameOfPos(1));
	}

	@Test
	public void testTransform() {
		final DataCollection<String> dc = new MockDataCollection<String>(
				new String[] { "FirstName", "SecondName" });

		
		final Collection<Object> tc = dc.transform();
		
		// TODO formalize test
	}
}
