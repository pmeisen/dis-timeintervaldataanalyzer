package net.meisen.dissertation;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xsd.TestXsdTidaModel;
import net.meisen.dissertation.config.xslt.TestDefaultValues;
import net.meisen.dissertation.config.xslt.TestXsltTidaModel;
import net.meisen.dissertation.help.TestDb;
import net.meisen.dissertation.impl.dataretriever.TestDbDataRetriever;
import net.meisen.dissertation.impl.dataretriever.TestFixedStructureDataRetriever;
import net.meisen.dissertation.impl.datasets.TestDataRetrieverDataSet;
import net.meisen.dissertation.impl.datasets.TestSingleStaticDataSet;
import net.meisen.dissertation.impl.descriptors.TestDoubleDescriptor;
import net.meisen.dissertation.impl.descriptors.TestGeneralDescriptor;
import net.meisen.dissertation.impl.descriptors.TestIntegerDescriptor;
import net.meisen.dissertation.impl.descriptors.TestLongDescriptor;
import net.meisen.dissertation.impl.idfactories.TestByteIdsFactory;
import net.meisen.dissertation.impl.idfactories.TestIntegerIdsFactory;
import net.meisen.dissertation.impl.idfactories.TestLongIdsFactory;
import net.meisen.dissertation.impl.idfactories.TestShortIdsFactory;
import net.meisen.dissertation.impl.idfactories.TestUuIdsFactory;
import net.meisen.dissertation.impl.indexes.TestCompositeIndexKey;
import net.meisen.dissertation.impl.indexes.TestIndexFactory;
import net.meisen.dissertation.impl.indexes.TestIndexedCollectionDefinition;
import net.meisen.dissertation.impl.indexes.TestIntArrayCollection;
import net.meisen.dissertation.impl.indexes.TestMapIndex;
import net.meisen.dissertation.impl.indexes.TestMultipleIndexedCollection;
import net.meisen.dissertation.impl.indexes.TestNestedIndexedCollection;
import net.meisen.dissertation.impl.indexes.datarecord.bitmap.TestEWAHBitmap;
import net.meisen.dissertation.impl.parser.query.TestQueryFactory;
import net.meisen.dissertation.impl.persistence.TestZipPersistor;
import net.meisen.dissertation.impl.time.granularity.TestTimeGranularityFactory;
import net.meisen.dissertation.impl.time.mapper.TestDateMapper;
import net.meisen.dissertation.model.data.TestDataModel;
import net.meisen.dissertation.model.data.TestDataStructure;
import net.meisen.dissertation.model.data.TestMetaDataModel;
import net.meisen.dissertation.model.dataretriever.TestDataCollection;
import net.meisen.dissertation.model.dataretriever.TestDataRecord;
import net.meisen.dissertation.model.datasets.TestMultipleDataSetIterator;
import net.meisen.dissertation.model.descriptors.TestDescriptorModel;
import net.meisen.dissertation.model.descriptors.TestDescriptorPrimitiveDataType;
import net.meisen.dissertation.model.descriptors.TestResourceDescriptor;
import net.meisen.dissertation.model.handler.TestTidaModelHandler;
import net.meisen.dissertation.model.indexes.TestIndexKeyDefinition;
import net.meisen.dissertation.model.indexes.datarecord.TestIntervalIndexPartition;
import net.meisen.dissertation.model.indexes.datarecord.TestMetaIndex;
import net.meisen.dissertation.model.indexes.datarecord.TestMetaIndexDimension;
import net.meisen.dissertation.model.persistence.TestBasePersistor;
import net.meisen.dissertation.model.persistence.TestGroup;
import net.meisen.dissertation.model.persistence.TestIdentifier;
import net.meisen.dissertation.model.time.TestDateNormalizer;
import net.meisen.dissertation.model.time.granularity.TestDateBasedHelper;
import net.meisen.dissertation.model.time.granularity.TestDateFormat;
import net.meisen.dissertation.model.time.granularity.TestMinute;
import net.meisen.dissertation.model.time.mapper.TestBaseMapperFactory;
import net.meisen.dissertation.model.time.timeline.TestTimelineDefinition;
import net.meisen.dissertation.server.TestTidaServer;

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

		// Test the utilities
		TestDefaultValues.class,

		// Test general helper
		TestDb.class,

		// Test the configuration
		TestConfig.TestConfigSuite.class,

		// Test persistence
		TestGroup.class,
		TestIdentifier.class,
		TestBasePersistor.class,
		TestZipPersistor.class,

		// Test indexes
		TestCompositeIndexKey.class,
		TestIndexKeyDefinition.class,
		TestMapIndex.class,
		TestIndexedCollectionDefinition.class,
		TestNestedIndexedCollection.class,
		TestMultipleIndexedCollection.class,
		TestIntArrayCollection.class,

		// Test the factory for the indexes
		TestIndexFactory.TestIndexFactorySuite.class,

		// Test the id factories
		TestByteIdsFactory.class,
		TestShortIdsFactory.class,
		TestIntegerIdsFactory.class,
		TestLongIdsFactory.class,
		TestUuIdsFactory.class,

		// Test the descriptors
		TestDescriptorPrimitiveDataType.class,
		TestDoubleDescriptor.class,
		TestIntegerDescriptor.class,
		TestLongDescriptor.class,
		TestGeneralDescriptor.class,
		TestResourceDescriptor.class,

		// Test the descriptorModel
		TestDescriptorModel.class,

		// Tests the formatting of dates, granularities, and the factory
		TestDateFormat.class,
		TestDateBasedHelper.class,
		TestMinute.class,
		TestTimeGranularityFactory.class,

		// Test the normalizer, the timeline and the mapper
		TestDateNormalizer.class,
		TestTimelineDefinition.class,
		TestDateMapper.class,

		// Test the time factory
		TestBaseMapperFactory.class,

		// Test the dataRetrievers
		TestDataCollection.class, TestDataRecord.class,
		TestDbDataRetriever.class,
		TestFixedStructureDataRetriever.class,

		// Test the dataSets
		TestSingleStaticDataSet.class, TestDataRetrieverDataSet.class,
		TestMultipleDataSetIterator.class,

		// Test the different created instances
		TestDataStructure.class, TestMetaDataModel.class, TestDataModel.class,

		// Test the xsd and xslt
		TestXsdTidaModel.class, TestXsltTidaModel.class,

		// Test some bitmap implementations
		TestEWAHBitmap.class,
		
		// Test the tida-indexes
		TestMetaIndexDimension.class, TestMetaIndex.class,
		TestIntervalIndexPartition.class,

		// Test the handler
		TestTidaModelHandler.class,

		// Test the query
		TestQueryFactory.class, 

		// Test the server
		TestTidaServer.class })
public class AllUnitTests {

}
