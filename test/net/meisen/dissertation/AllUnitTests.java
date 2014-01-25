package net.meisen.dissertation;

import net.meisen.dissertation.config.xsd.TestXsdTidaModel;
import net.meisen.dissertation.config.xslt.TestXsltTidaModel;
import net.meisen.dissertation.help.TestDb;
import net.meisen.dissertation.impl.dataretriever.TestDbDataRetriever;
import net.meisen.dissertation.impl.dataretriever.TestFixedStructureDataRetriever;
import net.meisen.dissertation.impl.descriptors.TestDoubleDescriptor;
import net.meisen.dissertation.impl.descriptors.TestGeneralDescriptor;
import net.meisen.dissertation.impl.descriptors.TestIntegerDescriptor;
import net.meisen.dissertation.impl.descriptors.TestLongDescriptor;
import net.meisen.dissertation.impl.idfactories.TestIntegerIdsFactory;
import net.meisen.dissertation.impl.idfactories.TestLongIdsFactory;
import net.meisen.dissertation.impl.idfactories.TestUuIdsFactory;
import net.meisen.dissertation.impl.indexes.TestCompositeIndexKey;
import net.meisen.dissertation.impl.indexes.TestIndexedCollectionDefinition;
import net.meisen.dissertation.impl.indexes.TestMapIndex;
import net.meisen.dissertation.impl.indexes.TestMultipleIndexedCollection;
import net.meisen.dissertation.impl.indexes.TestNestedIndexedCollection;
import net.meisen.dissertation.impl.naturals.TestBigIntegerNaturals;
import net.meisen.dissertation.impl.naturals.TestBigIntegerNaturalsFactory;
import net.meisen.dissertation.impl.naturals.TestIntegerNaturals;
import net.meisen.dissertation.impl.naturals.TestIntegerNaturalsFactory;
import net.meisen.dissertation.impl.naturals.TestLongNaturals;
import net.meisen.dissertation.impl.naturals.TestLongNaturalsFactory;
import net.meisen.dissertation.model.data.TestMetaDataModel;
import net.meisen.dissertation.model.dataretriever.TestDataCollection;
import net.meisen.dissertation.model.dataretriever.TestDataRecord;
import net.meisen.dissertation.model.datasets.TestDataRetrieverDataSet;
import net.meisen.dissertation.model.datasets.TestSingleStaticDataSet;
import net.meisen.dissertation.model.descriptors.TestDescriptorModel;
import net.meisen.dissertation.model.descriptors.TestDescriptorPrimitiveDataType;
import net.meisen.dissertation.model.descriptors.TestResourceDescriptor;
import net.meisen.dissertation.model.indexes.TestIndexKeyDefinition;
import net.meisen.dissertation.model.time.TestRawTemporalModel;
import net.meisen.dissertation.model.time.TestRawTimeIntervalFactory;
import net.meisen.dissertation.model.time.TestRawTimePointFactory;
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

		// Test the dataSets
		TestSingleStaticDataSet.class, TestDataRetrieverDataSet.class,
		
		// Test the different created instances
		TestMetaDataModel.class, 

		// Test the xsd and xslt
		TestXsdTidaModel.class, TestXsltTidaModel.class,

		// Test the server
		TestTIDAServer.class })
public class AllUnitTests {

}
