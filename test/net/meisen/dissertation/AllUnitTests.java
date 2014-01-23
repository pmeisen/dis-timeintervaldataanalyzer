package net.meisen.dissertation;

import net.meisen.dissertation.config.xsd.TestXsdTidaModel;
import net.meisen.dissertation.config.xslt.TestXsltTidaModel;
import net.meisen.dissertation.data.impl.dataretriever.TestDbDataRetriever;
import net.meisen.dissertation.data.impl.dataretriever.TestFixedStructureDataRetriever;
import net.meisen.dissertation.data.impl.descriptors.TestDoubleDescriptor;
import net.meisen.dissertation.data.impl.descriptors.TestGeneralDescriptor;
import net.meisen.dissertation.data.impl.descriptors.TestIntegerDescriptor;
import net.meisen.dissertation.data.impl.descriptors.TestLongDescriptor;
import net.meisen.dissertation.data.impl.idfactories.TestIntegerIdsFactory;
import net.meisen.dissertation.data.impl.idfactories.TestLongIdsFactory;
import net.meisen.dissertation.data.impl.idfactories.TestUuIdsFactory;
import net.meisen.dissertation.data.impl.indexes.TestCompositeIndexKey;
import net.meisen.dissertation.data.impl.indexes.TestIndexedCollectionDefinition;
import net.meisen.dissertation.data.impl.indexes.TestMapIndex;
import net.meisen.dissertation.data.impl.indexes.TestMultipleIndexedCollection;
import net.meisen.dissertation.data.impl.indexes.TestNestedIndexedCollection;
import net.meisen.dissertation.help.TestDb;
import net.meisen.dissertation.models.impl.data.TestDescriptorModel;
import net.meisen.dissertation.models.impl.data.TestDescriptorPrimitiveDataType;
import net.meisen.dissertation.models.impl.data.TestMetaDataModel;
import net.meisen.dissertation.models.impl.data.TestResourceDescriptor;
import net.meisen.dissertation.models.impl.data.TestTimeIntervalDataModel;
import net.meisen.dissertation.models.impl.dataretriever.TestDataCollection;
import net.meisen.dissertation.models.impl.dataretriever.TestDataRecord;
import net.meisen.dissertation.models.impl.indexes.TestIndexKeyDefinition;
import net.meisen.dissertation.models.impl.naturals.TestBigIntegerNaturals;
import net.meisen.dissertation.models.impl.naturals.TestBigIntegerNaturalsFactory;
import net.meisen.dissertation.models.impl.naturals.TestIntegerNaturals;
import net.meisen.dissertation.models.impl.naturals.TestIntegerNaturalsFactory;
import net.meisen.dissertation.models.impl.naturals.TestLongNaturals;
import net.meisen.dissertation.models.impl.naturals.TestLongNaturalsFactory;
import net.meisen.dissertation.models.impl.time.TestRawTemporalModel;
import net.meisen.dissertation.models.impl.time.TestRawTimeIntervalFactory;
import net.meisen.dissertation.models.impl.time.TestRawTimePointFactory;
import net.meisen.dissertation.server.TestTIDAServer;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * All tests together as a {@link Suite}
 * 
 * @author pmeisen
 * 
 */
@RunWith(Suite.class)
@Suite.SuiteClasses({

		// Test general helper
		TestDb.class,

		// Test the naturals
		TestIntegerNaturals.class,
		TestLongNaturals.class,
		TestBigIntegerNaturals.class,

		// Test the factories for naturals
		TestIntegerNaturalsFactory.class,
		TestLongNaturalsFactory.class,
		TestBigIntegerNaturalsFactory.class,

		// Test the factories for the point and interval
		TestRawTimePointFactory.class,
		TestRawTimeIntervalFactory.class,

		// Test the temporal model
		TestRawTemporalModel.class,

		// Test indexes
		TestCompositeIndexKey.class,
		TestIndexKeyDefinition.class,
		TestMapIndex.class,
		TestIndexedCollectionDefinition.class,
		TestNestedIndexedCollection.class,
		TestMultipleIndexedCollection.class,

		// Test the id factories
		TestIntegerIdsFactory.class,
		TestLongIdsFactory.class,
		TestUuIdsFactory.class,

		// Test the descriptors
		TestDescriptorPrimitiveDataType.class, TestDoubleDescriptor.class,
		TestIntegerDescriptor.class, TestLongDescriptor.class,
		TestGeneralDescriptor.class, TestResourceDescriptor.class,
		
		// Test the descriptorModel
		TestDescriptorModel.class,

		// Test the dataRetrievers
		TestDataCollection.class, TestDataRecord.class,
		TestDbDataRetriever.class, TestFixedStructureDataRetriever.class,

		// Test combined meta-model and interval-model
		TestMetaDataModel.class, TestTimeIntervalDataModel.class,

		// Test the xsd and xslt
		TestXsdTidaModel.class, TestXsltTidaModel.class,

		// Test the server
		TestTIDAServer.class })
public class AllUnitTests {

}
