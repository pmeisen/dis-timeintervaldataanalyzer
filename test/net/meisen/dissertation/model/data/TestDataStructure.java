package net.meisen.dissertation.model.data;

import static org.junit.Assert.assertEquals;

import net.meisen.dissertation.model.datastructure.MetaStructureEntry;

import org.junit.Test;

public class TestDataStructure {

	/**
	 * Tests the usage of an empty {@code DataStructure}.
	 */
	@Test
	public void testEmptyStructure() {
		final DataStructure ds = new DataStructure();
		assertEquals(0, ds.getSize());
	}

	@Test
	public void testStructure() {
		final MetaStructureEntry metaD1Entry = new MetaStructureEntry("D1", 1);
		final MetaStructureEntry metaD2Entry = new MetaStructureEntry("D2", 2);
		final MetaStructureEntry metaD3Entry = new MetaStructureEntry("D3", 3);

		final DataStructure ds = new DataStructure(metaD1Entry, metaD2Entry,
				metaD3Entry);
		assertEquals(3, ds.getSize());
	}
}
