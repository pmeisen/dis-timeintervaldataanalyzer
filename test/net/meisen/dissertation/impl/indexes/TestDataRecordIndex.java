package net.meisen.dissertation.impl.indexes;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;

import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.impl.persistence.FileLocation;
import net.meisen.dissertation.model.data.TidaModel;

import org.junit.Test;

/**
 * Tests the implementation of the {@code DataRecordIndex}.
 * 
 * @author pmeisen
 * 
 */
public class TestDataRecordIndex extends LoaderBasedTest {

	/**
	 * Tests the saving and loading of the records stored in the
	 * {@code DataRecordIndex}.
	 * 
	 * @throws IOException
	 *             if a file cannot be created
	 */
	@Test
	public void testSaveAndLoad() throws IOException {
		TidaModel model;

		// load the model and unload it again
		model = m("/net/meisen/dissertation/impl/indexes/testDataRecordIndex.xml");
		assertEquals(4, model.getDataRecordCache().size());

		// save the model
		final File tmpFile = File.createTempFile("testDataRecordIndex", ".zip");
		loader.save(model.getId(), new FileLocation(tmpFile));

		// unload the model
		loader.unload(model.getId());

		// reload it from the file
		model = loader.load(new FileLocation(tmpFile));
		assertEquals(4, model.getDataRecordCache().size());

		// cleanUp
		loader.unload(model.getId());
		assertTrue(tmpFile.delete());
	}
}
