package net.meisen.dissertation.model.indexes.datarecord;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;

import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.descriptors.Descriptor;

/**
 * Tests the implementation of the {@code TidaIndex}.
 * 
 * @author pmeisen
 * 
 */
public class TestTidaIndex extends LoaderBasedTest {

	/**
	 * Tests the implementation of
	 * {@link TidaIndex#getDescriptorOfRecord(String, int)}.
	 */
	@Test
	public void testGetDescriptorOfRecord() {
		final TidaModel m = m("/net/meisen/dissertation/model/indexes/datarecord/tidaStaticMetaIndex.xml");

		final Descriptor<?, ?, ?> holgDesc = m.getMetaDataModel()
				.getDescriptorByValue("FAMILY", "Holger");
		final Descriptor<?, ?, ?> hajoDesc = m.getMetaDataModel()
				.getDescriptorByValue("FAMILY", "Hajo");
		final Descriptor<?, ?, ?> hannDesc = m.getMetaDataModel()
				.getDescriptorByValue("FAMILY", "Hannah");

		// get the subject
		final TidaIndex idx = m.getIndex();

		// get some descriptors
		assertEquals(holgDesc, idx.getDescriptorOfRecord("FAMILY", 0));
		assertEquals(hajoDesc, idx.getDescriptorOfRecord("FAMILY", 1));
		assertEquals(hannDesc, idx.getDescriptorOfRecord("FAMILY", 2));

		// use some invalid identifiers
		assertNull(idx.getDescriptorOfRecord("FAMILY", -1));
		assertNull(idx.getDescriptorOfRecord("FAMILY",
				idx.getLastRecordId() + 1));
	}
}
