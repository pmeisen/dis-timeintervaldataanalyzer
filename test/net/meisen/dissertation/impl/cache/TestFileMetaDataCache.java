package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.data.metadata.MetaDataCollection;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.genmisc.types.Streams;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Tests the implementation of the {@code FileMetaDataCache}.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
public class TestFileMetaDataCache extends ModuleBasedTest {

	private TidaModel model = null;
	private File tmpFile = new File(System.getProperty("java.io.tmpdir"),
			"testMetaCache");

	/**
	 * Makes sure a temporary file is defined and not created yet.
	 */
	@Before
	public void init() {
		cleanUp();
	}

	/**
	 * Tests the implementation of
	 * {@link FileMetaDataCache#write(File, MetaDataCollection)} and
	 * {@link FileMetaDataCache#read(File)} when an empty collection is written
	 * and read.
	 */
	@Test
	public void testWriteAndReadOfEmptyCollection() {

		// create the collection to be written
		final MetaDataCollection collection = new MetaDataCollection();

		// check some pre-requirements
		assertFalse(tmpFile.exists());

		// create the file
		final FileMetaDataCache mdc = new FileMetaDataCache();
		mdc.write(tmpFile, collection);

		final MetaDataCollection readCollection = mdc.read(tmpFile);
		assertEquals(collection, readCollection);
	}

	/**
	 * Tests the implementation of
	 * {@link FileMetaDataCache#write(File, MetaDataCollection)} and
	 * {@link FileMetaDataCache#read(File)}.
	 */
	@Test
	public void testWriteAndReadOfMetaModelData() {
		setModulesHolder("/net/meisen/dissertation/impl/cache/fileMetaDataCache.xml");

		// get the model and make sure it's not initialized
		model = modulesHolder.getModule(DefaultValues.TIDAMODEL_ID);
		assertFalse(model.isInitialized());

		// create the collection to be written
		final MetaDataCollection collection = UtilMetaDataCache
				.createCollectionForModel(model.getMetaDataModel());

		// check some pre-requirements
		assertFalse(tmpFile.exists());

		// create the file
		final FileMetaDataCache mdc = new FileMetaDataCache();
		mdc.write(tmpFile, collection);
		assertTrue(tmpFile.exists());
		assertTrue(tmpFile.canWrite());

		final MetaDataCollection readCollection = mdc.read(tmpFile);
		assertEquals(collection, readCollection);
	}

	/**
	 * Tests the usage of the {@code FileMetaDataCache} within a
	 * {@code TidaModel}, i.e. if the data is correctly reloaded from the file.
	 */
	@Test
	public void testReloadingCapabilities() {
		setModulesHolder("/net/meisen/dissertation/impl/cache/fileMetaDataCache.xml");

		// get the model and make sure it's not initialized
		model = modulesHolder.getModule(DefaultValues.TIDAMODEL_ID);

		// get the NULL-Descriptor now, so that it gets the first id
		final MetaDataModel metaModel = model.getMetaDataModel();
		metaModel.getDescriptorModel("AIRLINE").getNullDescriptor();

		// initialize and release the model, but don't delete anything from it
		model.initialize();

		// check the sizes
		assertEquals(3, metaModel.getDescriptorModel("AIRLINE").sizeAll());
		assertEquals(0, metaModel.getDescriptorModel("PAX").sizeAll());
		assertEquals(2, metaModel.getDescriptorModel("CREW").sizeAll());

		// make sure we have some mixed up identifiers
		assertEquals(1, metaModel.getDescriptorModel("AIRLINE")
				.getNullDescriptor().getId());
		assertEquals(2, metaModel.getDescriptorModel("CREW")
				.getNullDescriptor().getId());

		// release the model
		model.release();

		// load the model again
		setModulesHolder("/net/meisen/dissertation/impl/cache/fileMetaDataCache.xml");
		model = modulesHolder.getModule(DefaultValues.TIDAMODEL_ID);
		final MetaDataModel reloadedMetaModel = model.getMetaDataModel();

		// initialize the model
		model.initialize();

		// check the sizes
		assertEquals(3, reloadedMetaModel.getDescriptorModel("AIRLINE")
				.sizeAll());
		assertEquals(0, reloadedMetaModel.getDescriptorModel("PAX").sizeAll());
		assertEquals(2, reloadedMetaModel.getDescriptorModel("CREW").sizeAll());

		// check if the identifiers are also mixed
		assertEquals(1, reloadedMetaModel.getDescriptorModel("AIRLINE")
				.getNullDescriptor().getId());
		assertEquals(2, reloadedMetaModel.getDescriptorModel("CREW")
				.getNullDescriptor().getId());
	}

	/**
	 * Helper method to create an empty or corrupted metaDataFile.
	 * 
	 * @param file
	 *            the file to be created
	 * @param corrupt
	 *            {@code true} if the file should be corrupted, otherwise
	 *            {@code false} which creates an empty file
	 * 
	 * @throws IOException
	 *             if the file cannot be created
	 */
	protected void createFile(final File file, final boolean corrupt)
			throws IOException {

		// create a corrupted backup
		file.getParentFile().mkdirs();
		file.delete();
		assertFalse(file.exists());
		file.createNewFile();
		assertTrue(file.exists());

		if (corrupt) {
			final FileOutputStream fis = new FileOutputStream(file);
			fis.getChannel().write(ByteBuffer.wrap(new byte[] { 0, 1, 2 }));

			// release the file
			Streams.closeIO(fis);
		}
	}

	/**
	 * Tests the exception to be thrown if a corrupted file is read
	 * 
	 * @throws IOException
	 *             if a test-file cannot be created
	 */
	@Test
	public void testExceptionCorruption() throws IOException {
		thrown.expect(FileMetaDataCacheException.class);
		thrown.expectMessage("is corrupted");

		setModulesHolder("/net/meisen/dissertation/impl/cache/fileMetaDataCache.xml");

		// get the model and make sure it's not initialized
		model = modulesHolder.getModule(DefaultValues.TIDAMODEL_ID);

		// create a corrupt file
		final File file = new File(model.getLocation(),
				FileMetaDataCache.metaDataFileName);
		createFile(file, true);

		// initialize the model
		model.initialize();
	}

	/**
	 * Tests the fallback to a backup.
	 * 
	 * @throws IOException
	 *             if the testfiles cannot be created
	 */
	@Test
	public void testFallback() throws IOException {
		setModulesHolder("/net/meisen/dissertation/impl/cache/fileMetaDataCache.xml");

		// get the model and make sure it's not initialized
		model = modulesHolder.getModule(DefaultValues.TIDAMODEL_ID);

		// create an empty file
		final File backupFile = new File(model.getLocation(),
				FileMetaDataCache.metaDataFileName
						+ FileMetaDataCache.backupFileExtension);
		createFile(backupFile, false);

		// create a corrupt file
		final File file = new File(model.getLocation(),
				FileMetaDataCache.metaDataFileName);
		createFile(file, true);

		// initialize the model
		model.initialize();

		// read the metaModel
		final MetaDataModel metaModel = model.getMetaDataModel();
		assertEquals(3, metaModel.getDescriptorModels().size());
		assertEquals(0, metaModel.getDescriptorModel("AIRLINE").size());
		assertEquals(0, metaModel.getDescriptorModel("CREW").size());
		assertEquals(0, metaModel.getDescriptorModel("PAX").size());
	}

	/**
	 * If a module was loaded any created folder is deleted.
	 */
	@After
	public void cleanUp() {
		if (model != null) {
			model.release(true);
		}
		if (tmpFile.exists()) {
			assertTrue(Files.deleteDir(tmpFile));
		}
	}
}
