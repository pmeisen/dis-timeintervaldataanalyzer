package net.meisen.dissertation.model.indexes.datarecord;

import static org.junit.Assert.assertEquals;
import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.help.ModuleAndDbBasedTest;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IClosableIterator;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.datastructure.MetaStructureEntry;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.hamcrest.Description;
import org.junit.After;
import org.junit.Test;
import org.junit.internal.matchers.TypeSafeMatcher;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Tests the implementation of a {@code MetaIndex}.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TidaConfig.class)
@ContextFile("sbconfigurator-core.xml")
public class TestMetaIndex extends ModuleAndDbBasedTest {

	@Autowired
	private TidaModelHandler loader;

	/**
	 * Tests the usage when loading a static model.
	 */
	@Test
	public void testUsingStaticIndexModel() {
		final TidaModel model = loader
				.loadViaXslt("/net/meisen/dissertation/model/indexes/datarecord/tidaStaticMetaIndex.xml");

		final MetaIndex metaIndex = new MetaIndex(model);
		final IClosableIterator<IDataRecord> it = model.getDataModel()
				.iterator();
		int i = 0;
		while (it.hasNext()) {
			final IDataRecord rec = it.next();
			final ProcessedDataRecord dataRec = new ProcessedDataRecord(rec,
					model, i);
			metaIndex.index(dataRec);
			i++;
		}
		it.close();

		// check if we got all the dimensions
		assertEquals(1, metaIndex.getAmountOfDimensions());
	}

	/**
	 * Tests the usage when loading a static model.
	 */
	@Test
	public void testUsingRandomIndexModel() {
		final TidaModel model = loader
				.loadViaXslt("/net/meisen/dissertation/model/indexes/datarecord/tidaRandomMetaIndex.xml");

		// create the indexes
		final MetaIndex metaIndex = new MetaIndex(model);
		final IClosableIterator<IDataRecord> it = model.getDataModel()
				.iterator();
		int i = 0;
		while (it.hasNext()) {
			final IDataRecord rec = it.next();
			final ProcessedDataRecord dataRec = new ProcessedDataRecord(rec,
					model, i);

			// add the record
			metaIndex.index(dataRec);
			i++;
		}
		it.close();

		// check if we got all the dimensions
		assertEquals(2, metaIndex.getAmountOfDimensions());
	}

	/**
	 * Tests the creation of a {@code MetaIndexDimension} using {@code null} as
	 * {@code MetaStructureEntry}, which should lead to an exception.
	 */
	@Test
	public void testExceptionNullCreateDimension() {
		thrown.expect(NullPointerException.class);
		thrown.expectMessage("The metaEntry cannot be null.");

		final TidaModel model = loader
				.loadViaXslt("/net/meisen/dissertation/model/indexes/datarecord/tidaRandomMetaIndex.xml");

		// create the indexes
		final MetaIndex metaIndex = new MetaIndex(model);
		metaIndex.createIndexDimension(null, model);
	}

	/**
	 * Tests the creation of a {@code MetaIndexDimension} referring to an
	 * invalid {@code DescriptorModel}.
	 */
	@Test
	public void testExceptionCreatingForInvalidModel() {
		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new TypeSafeMatcher<ForwardedRuntimeException>() {
			private final String expected = "Number: '1001'";

			@Override
			public void describeTo(final Description description) {
				description.appendText(expected);
			}

			@Override
			public boolean matchesSafely(final ForwardedRuntimeException item) {
				return item.toString().contains(expected);
			}
		});

		final TidaModel model = loader
				.loadViaXslt("/net/meisen/dissertation/model/indexes/datarecord/tidaRandomMetaIndex.xml");

		// create the indexes
		final MetaIndex metaIndex = new MetaIndex(model);
		final MetaStructureEntry metaEntry = new MetaStructureEntry(
				"ID_UNKNOWN", "myName");
		metaIndex.createIndexDimension(metaEntry, model);
	}

	/**
	 * CleanUp after test
	 */
	@After
	public void unloadAll() {
		loader.unloadAll();
	}
}
