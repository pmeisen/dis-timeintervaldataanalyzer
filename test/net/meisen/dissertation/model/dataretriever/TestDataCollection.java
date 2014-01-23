package net.meisen.dissertation.model.dataretriever;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Collection;

import net.meisen.dissertation.model.dataretriever.DataCollection;
import net.meisen.dissertation.model.dataretriever.mock.MockDataCollection;
import net.meisen.general.genmisc.collections.Collections;

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

	/**
	 * Tests the implementation of {@link DataCollection#transform(int)}.
	 */
	@Test
	public void testTransform() {
		final DataCollection<String> dc = new MockDataCollection<String>(
				new String[] { "FirstName", "SecondName" }, 200);

		Collection<Object> tc;

		tc = dc.transform("FirstName");
		for (int i = 0; i < 200; i++) {
			assertEquals("FirstName " + i, Collections.get(i, tc));
		}

		tc = dc.transform(null);
		for (int i = 0; i < 200; i++) {
			assertEquals("FirstName " + i, Collections.get(i, tc));
		}

		tc = dc.transform("SecondName");
		for (int i = 0; i < 200; i++) {
			assertEquals("SecondName " + i, Collections.get(i, tc));
		}

		tc = dc.transform(0);
		for (int i = 0; i < 200; i++) {
			assertEquals("FirstName " + i, Collections.get(i, tc));
		}

		tc = dc.transform(1);
		for (int i = 0; i < 200; i++) {
			assertEquals("SecondName " + i, Collections.get(i, tc));
		}
	}
}
