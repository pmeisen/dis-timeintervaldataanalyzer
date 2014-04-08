package net.meisen.dissertation.impl.persistence;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.UUID;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.ZipPersistorException;
import net.meisen.dissertation.help.ExceptionBasedTest;
import net.meisen.dissertation.impl.persistence.mock.MockLoadSinglePersistable;
import net.meisen.dissertation.impl.persistence.mock.MockSaveSinglePersistable;
import net.meisen.dissertation.model.persistence.Group;
import net.meisen.dissertation.model.persistence.Identifier;
import net.meisen.dissertation.model.persistence.MetaData;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.genmisc.types.Streams;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the implementation of a {@code ZipPersistor}.
 * 
 * @author pmeisen
 * 
 * @see ZipPersistor
 * 
 */
@RunWith(JUnitConfigurationRunner.class)
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
@SystemProperty(property = "testBeans.selector", value = "?")
public class TestZipPersistor extends ExceptionBasedTest {

	@Autowired(required = true)
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	private File tmpDir;
	private File tmpFile;
	private ZipPersistor persistor;

	/**
	 * Setups a temporary file to be used as well as a {@code ZipPersistor}
	 * instance.
	 * 
	 * @throws IOException
	 *             if the file cannot be created
	 */
	@Before
	public void setUp() throws IOException {
		tmpDir = new File(System.getProperty(("java.io.tmpdir")));
		tmpFile = File.createTempFile(UUID.randomUUID().toString(), ".zip");
		persistor = new ZipPersistor(exceptionRegistry);

		// make sure the file got created correctly
		assertTrue(tmpFile.exists());
		assertTrue(tmpFile.isFile());
	}

	/**
	 * Tests the creation of the saving location.
	 */
	@Test
	public void testFileCreation() {

		// try to use an existing file which should be deleteable
		persistor.save(new FileLocation(tmpFile));

		// create another temporary file in a sub-directory
		final File fileWithSubDirectory = new File(tmpFile.getParent(),
				"aSubDirectory/aFile.zip");
		persistor.save(new FileLocation(fileWithSubDirectory));
		assertTrue(fileWithSubDirectory.exists());
		assertTrue(fileWithSubDirectory.isFile());
		assertTrue(Files.deleteDir(fileWithSubDirectory.getParentFile()));
	}

	/**
	 * Tests the exception expected when using an invalid filename without any
	 * folder.
	 */
	@Test
	public void testInvalidFilenameWithoutFolderException() {
		thrown.expect(ZipPersistorException.class);
		thrown.expectMessage("The storage-location '?' could not be created.");

		persistor.save(new FileLocation(new File("?")));
	}

	/**
	 * Tests the exception expected when using an invalid filename with folder.
	 */
	@Test
	public void testInvalidFilenameWithFolderException() {
		thrown.expect(ZipPersistorException.class);
		thrown.expectMessage("Unable to access or create the file");

		persistor.save(new FileLocation(new File(tmpDir, "?")));
	}

	/**
	 * Tests the saving of a {@code null} location.
	 */
	@Test
	public void testSaveNullException() {
		thrown.expect(ZipPersistorException.class);
		thrown.expectMessage("A location must be specified.");

		persistor.save(null);
	}

	/**
	 * Tests the usage of a file which is in use.
	 * 
	 * @throws IOException
	 *             the exception if the file cannot be created
	 */
	@Test
	public void testFileInUseException() throws IOException {
		tmpFile.createNewFile();
		final InputStream stream = new FileInputStream(tmpFile);
		try {
			persistor.save(new FileLocation(tmpFile));
			fail("Exception not thrown.");
		} catch (final ZipPersistorException e) {
			assertTrue(e
					.getMessage()
					.contains(
							"cannot be used as storage-location, because it already exists and cannot be removed"));
		} finally {
			Streams.closeIO(stream);
		}
	}

	/**
	 * Tests the saving process.
	 * 
	 * @throws IOException
	 *             if the file cannot be read
	 */
	@Test
	public void testSaving() throws IOException {

		// we need to register some persistables
		persistor.register(new Group("my", "group"),
				new MockSaveSinglePersistable());

		// save the file
		persistor.save(new FileLocation(tmpFile));

		// check the result
		final ZipFile file = new ZipFile(tmpFile);
		assertEquals(1, file.size());

		final ZipEntry entry = file.entries().nextElement();
		assertEquals("my/group/" + MockSaveSinglePersistable.ID,
				entry.getName());
		final String content = Streams.readFromStream(file
				.getInputStream(entry));
		assertEquals(MockSaveSinglePersistable.CONTENT, content);

		// close the file and give it free
		file.close();
	}

	/**
	 * Tests the loading of a saved instance.
	 */
	@Test
	public void testLoading() {
		final Group group = new Group("please", "load", "me");

		// we need to register some persistables
		persistor.register(group, new MockSaveSinglePersistable());

		// save the file
		persistor.save(new FileLocation(tmpFile));

		// now load the file
		final MockLoadSinglePersistable loader = new MockLoadSinglePersistable();
		persistor.unregister(group);
		persistor.register(group, loader);
		persistor.load(new FileLocation(tmpFile));

		// check the result that was loaded
		assertEquals(MockSaveSinglePersistable.CONTENT, loader.getContent());
	}

	/**
	 * Tests the saving of a single {@code MetaData} instance
	 * 
	 * @throws IOException
	 *             if the file cannot be read
	 */
	@Test
	public void testSavingOfSingleMetaData() throws IOException {
		final String name = "meta.info";
		final String content = "This is some MetaData with weird symbols üöäß";

		persistor.save(new FileLocation(tmpFile), new MetaData(new Identifier(
				name), content));

		// check the result
		final ZipFile file = new ZipFile(tmpFile);
		assertEquals(1, file.size());

		final ZipEntry entry = file.entries().nextElement();
		assertEquals(name, entry.getName());
		final String readContent = Streams.readFromStream(file
				.getInputStream(entry));
		assertEquals(content, readContent);

		// close the handler
		file.close();
	}

	/**
	 * Tests the saving of multiple {@code MetaData} instances.
	 * 
	 * @throws IOException
	 *             if the file cannot be read
	 */
	@Test
	public void testSavingOfMultipleMetaData() throws IOException {

		// @formatter:off
		MetaData[] data = new MetaData[] { 
				new MetaData(new Identifier("myName.text", "group"), "This is a string"),
				new MetaData(new Identifier("another.file", "group"), new ByteArrayInputStream("Another String".getBytes())),
				new MetaData(new Identifier("suck")) };
		// @formatter:on

		persistor.save(new FileLocation(tmpFile), data);

		// check the result
		final ZipFile file = new ZipFile(tmpFile);
		assertEquals(3, file.size());
		assertEquals("This is a string", Streams.readFromStream(file
				.getInputStream(file.getEntry("group/myName.text"))));
		assertEquals("Another String", Streams.readFromStream(file
				.getInputStream(file.getEntry("group/another.file"))));
		assertEquals("", Streams.readFromStream(file.getInputStream(file
				.getEntry("suck"))));

		// close the handler
		file.close();
	}

	/**
	 * Delete the created temporary file
	 */
	@After
	public void cleanUp() {
		assertTrue(tmpFile.delete());
	}
}
