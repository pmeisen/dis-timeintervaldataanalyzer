package net.meisen.dissertation.bugs.bug0001;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.impl.dataretriever.CsvDataConfig;
import net.meisen.dissertation.impl.dataretriever.CsvDataRetriever;
import net.meisen.dissertation.impl.datasets.DataRetrieverDataSet;
import net.meisen.dissertation.model.data.DataStructure;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry;
import net.meisen.dissertation.model.datastructure.MetaStructureEntry;
import net.meisen.dissertation.model.datastructure.StructureEntry;
import net.meisen.dissertation.model.descriptors.IDescriptorFactory;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Bug 0001: The bug addresses a problem with the DoubleDescriptor. It seems,
 * that the DescriptorModel cannot find a specific descriptor. <br />
 * <br />
 * A first analysis showed that the problem is based on the different string
 * representation (i.e. 1.00, 1, 1.0, ...). The csv-file provides a string which
 * presents the double as 1.00, which will be transformed internally to 1. What
 * we need is a single formatting instance for descriptor values. Main problem,
 * there is no instance available within the model.<br />
 * <br />
 * To solve the problem, we introduce the possibility to define a factory for a
 * {@code Descriptor}. The main problem is, that Java (or probably our design)
 * uses the {@code DescriptorModel} as factory for any kind of
 * {@code Descriptor}. Thus, a special factory for a concrete {@code Descriptor}
 * implementation is class-bound. Class-bound things cannot be defined within
 * any interface, neither in any abstract class. So we decided to add the
 * possibility to define a factory as nested static class (being aware of the
 * consequences considering "bad design"). Thus, the bug is resolved and
 * everything should just work fine.
 * 
 * @author pmeisen
 * 
 * @see IDescriptorFactory
 * 
 */
public class TestForBug0001 extends LoaderBasedTest {

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry registry;

	/**
	 * The execution of the method throws the unexpected exception:<br/>
	 * {@code  net.meisen.dissertation.exceptions.DescriptorModelException:}<br/>
	 * {@code The
	 * descriptor 'DoubleDescriptor: 1' cannot be added to the model, because
	 * another descriptor 'null' with the same identifier or value is already
	 * added.}
	 */
	@Test
	public void testFailure() {
		TidaModel model = m("/net/meisen/dissertation/bugs/bug0001/callsModel.xml");

		// get the csv settings
		final CsvDataConfig config = new CsvDataConfig();
		config.setClasspath(true);
		config.setFile("/net/meisen/dissertation/bugs/bug0001/calls.csv");
		config.setSeparator(";");

		final List<StructureEntry> entries = new ArrayList<StructureEntry>();
		entries.add(new MetaStructureEntry("caller", "caller"));
		entries.add(new MetaStructureEntry("recipient", "recipient"));
		entries.add(new MetaStructureEntry("origin", "origin"));
		entries.add(new MetaStructureEntry("destination", "destination"));
		entries.add(new MetaStructureEntry("rate", "ratepermin"));
		entries.add(new IntervalStructureEntry("start", "start"));
		entries.add(new IntervalStructureEntry("end", "end"));
		final DataStructure structure = new DataStructure(entries);

		// get the data
		CsvDataRetriever retriever = null;
		try {

			// get the retriever
			retriever = new CsvDataRetriever(UUID.randomUUID().toString(),
					config);
			retriever.setExceptionRegistry(registry);

			// use it to load data
			final DataRetrieverDataSet dataSet = new DataRetrieverDataSet(
					retriever, null);

			// do the loading
			model.bulkLoadData(structure, dataSet.iterator());
		} finally {

			// release all db resources
			if (retriever != null) {
				retriever.release();
			}
		}
	}

}
