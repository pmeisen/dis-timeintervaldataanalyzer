package net.meisen.dissertation.model.data;

import static org.junit.Assert.assertEquals;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry;
import net.meisen.dissertation.model.datastructure.MetaStructureEntry;
import net.meisen.dissertation.model.datastructure.StructureEntry;

import org.junit.Test;

/**
 * Tests the implementation of a {@code DataStructure}.
 * 
 * @author pmeisen
 * 
 */
public class TestDataStructure {

	/**
	 * Tests the usage of an empty {@code DataStructure}.
	 */
	@Test
	public void testEmptyStructure() {
		final DataStructure ds = new DataStructure();
		assertEquals(0, ds.getSize());
	}

	/**
	 * Tests the implementations of methods for a {@code DataStructue}.
	 */
	@Test
	public void testStructure() {
		final MetaStructureEntry metaD1Entry = new MetaStructureEntry("D1", 1);
		final MetaStructureEntry metaD2Entry = new MetaStructureEntry("D2", 2);
		final MetaStructureEntry metaD3Entry = new MetaStructureEntry("D3", 3);

		// create the structure
		final DataStructure ds = new DataStructure(metaD1Entry, metaD2Entry,
				metaD3Entry);

		// check getSize
		assertEquals(3, ds.getSize());

		// check getEntriesByClass
		assertEquals(0, ds.getEntriesByClass(IntervalStructureEntry.class)
				.size());
		assertEquals(3, ds.getEntriesByClass(MetaStructureEntry.class).size());

		StructureEntry entry;
		entry = ds.getEntriesByClass(MetaStructureEntry.class).get(0);
		assertEquals(metaD1Entry.getName(), entry.getName());
		assertEquals(metaD1Entry, entry);

		entry = ds.getEntriesByClass(MetaStructureEntry.class).get(1);
		assertEquals(metaD2Entry.getName(), entry.getName());
		assertEquals(metaD2Entry, entry);

		entry = ds.getEntriesByClass(MetaStructureEntry.class).get(2);
		assertEquals(metaD3Entry.getName(), entry.getName());
		assertEquals(metaD3Entry, entry);
	}
}
