package net.meisen.dissertation.model.datasets;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Iterator;

import net.meisen.dissertation.impl.dataretriever.FixedStructureDataRetriever;
import net.meisen.dissertation.impl.dataretriever.FixedStructureQueryConfig;

import org.junit.Test;

/**
 * Tests the implementation of a {@code DataRetrieverDataSet}
 * 
 * @author pmeisen
 * 
 * @see DataRetrieverDataSet
 * 
 */
public class TestDataRetrieverDataSet {

	/**
	 * Tests the creation of a {@code DataRetrieverDataSet} with a {@code null}
	 * {@code BaseDataRetriever} and a {@code null} query.
	 */
	@Test
	public void testNullRetrieverAndQuery() {
		final DataRetrieverDataSet retriever = new DataRetrieverDataSet(null,
				null);

		// check some names
		assertFalse(retriever.hasNamedValue(null));
		assertFalse(retriever.hasNamedValue(""));
		assertFalse(retriever.hasNamedValue("TESTVALUE"));

		// check some positions
		for (int i = -100; i < 100; i++) {
			assertFalse(retriever.isValidPosition(i));
		}

		assertNotNull(retriever.getCollection());
		assertEquals(0, retriever.getCollection().get().size());
	}

	/**
	 * Tests the usage of the {@code DataSet} with a specified
	 * {@code DataRetriever}.
	 */
	@Test
	public void testUsageWithDataRetriever() {
		final FixedStructureDataRetriever fsDataRetriever = new FixedStructureDataRetriever();
		final DataRetrieverDataSet retriever = new DataRetrieverDataSet(
				fsDataRetriever, new FixedStructureQueryConfig(100));

		// check some names
		assertFalse(retriever.hasNamedValue(null));
		assertFalse(retriever.hasNamedValue(""));
		assertTrue(retriever
				.hasNamedValue(FixedStructureDataRetriever.DEF_NAME));

		// check some positions
		for (int i = -100; i < 100; i++) {
			assertEquals(i == 1, retriever.isValidPosition(i));
		}

		// check the data access and the available data
		final Iterator<IDataRecord> it = retriever.iterate();
		int counter = 0;
		while (it.hasNext()) {
			final IDataRecord data = it.next();
			assertTrue(data.hasNamedValue(FixedStructureDataRetriever.DEF_NAME));
			assertTrue(data.isValidPosition(1));
			assertEquals(data.getValue(1),
					data.getValue(FixedStructureDataRetriever.DEF_NAME));
			counter++;
		}
		assertEquals(100, counter);
	}
}
