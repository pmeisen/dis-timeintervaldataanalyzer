package net.meisen.dissertation;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xsd.TestXsdTidaModel;
import net.meisen.dissertation.config.xslt.TestDefaultValues;
import net.meisen.dissertation.config.xslt.TestXsltTidaModel;
import net.meisen.dissertation.help.TestDb;
import net.meisen.dissertation.impl.auth.shiro.TestMapDbAuthorizingRealm;
import net.meisen.dissertation.impl.auth.shiro.TestShiroAuthManager;
import net.meisen.dissertation.impl.cache.TestRecentlyUsedCachingStrategy;
import net.meisen.dissertation.impl.cache.TestFileBitmapCache;
import net.meisen.dissertation.impl.cache.TestFileBitmapIdCacheConfig;
import net.meisen.dissertation.impl.cache.TestFileCaches;
import net.meisen.dissertation.impl.cache.TestFileFactDescriptorModelSetCache;
import net.meisen.dissertation.impl.cache.TestFileIdentifierCache;
import net.meisen.dissertation.impl.cache.TestFileMetaDataCache;
import net.meisen.dissertation.impl.cache.TestIdsOnlyDataRecordCache;
import net.meisen.dissertation.impl.cache.TestMapDbBitmapCache;
import net.meisen.dissertation.impl.cache.TestMapDbDataRecordCache;
import net.meisen.dissertation.impl.cache.TestMemoryIdentifierCache;
import net.meisen.dissertation.impl.cache.TestMemoryMetaDataCache;
import net.meisen.dissertation.impl.dataintegration.TestPreProcessedDataRecord;
import net.meisen.dissertation.impl.dataintegration.TestScriptPreProcessor;
import net.meisen.dissertation.impl.dataretriever.TestCsvDataRetriever;
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
import net.meisen.dissertation.impl.indexes.TestContinuousIntIndexedCollection;
import net.meisen.dissertation.impl.indexes.TestDataRecordIndex;
import net.meisen.dissertation.impl.indexes.TestIndexFactory;
import net.meisen.dissertation.impl.indexes.TestIndexedCollectionDefinition;
import net.meisen.dissertation.impl.indexes.TestIntArrayCollection;
import net.meisen.dissertation.impl.indexes.TestMapIndex;
import net.meisen.dissertation.impl.indexes.TestMultipleIndexedCollection;
import net.meisen.dissertation.impl.indexes.TestNestedIndexedCollection;
import net.meisen.dissertation.impl.indexes.datarecord.slices.TestBitmapId;
import net.meisen.dissertation.impl.indexes.datarecord.slices.TestEWAHBitmap;
import net.meisen.dissertation.impl.indexes.datarecord.slices.TestRoaringBitmap;
import net.meisen.dissertation.impl.measures.TestMapFactsArrayBased;
import net.meisen.dissertation.impl.measures.TestMapFactsDescriptorBased;
import net.meisen.dissertation.impl.parser.query.TestAddQueries;
import net.meisen.dissertation.impl.parser.query.TestAliveQueries;
import net.meisen.dissertation.impl.parser.query.TestAssignQueries;
import net.meisen.dissertation.impl.parser.query.TestDeleteQueries;
import net.meisen.dissertation.impl.parser.query.TestDropQueries;
import net.meisen.dissertation.impl.parser.query.TestGetQueries;
import net.meisen.dissertation.impl.parser.query.TestGrantQueries;
import net.meisen.dissertation.impl.parser.query.TestInsertQueries;
import net.meisen.dissertation.impl.parser.query.TestLoadQueries;
import net.meisen.dissertation.impl.parser.query.TestModifyQueries;
import net.meisen.dissertation.impl.parser.query.TestRemoveQueries;
import net.meisen.dissertation.impl.parser.query.TestRevokeQueries;
import net.meisen.dissertation.impl.parser.query.TestSelectQueries;
import net.meisen.dissertation.impl.parser.query.TestUnloadQueries;
import net.meisen.dissertation.impl.parser.query.select.TestDescriptorComperator;
import net.meisen.dissertation.impl.parser.query.select.TestDescriptorValue;
import net.meisen.dissertation.impl.parser.query.select.evaluator.TestDescriptorLogicEvaluator;
import net.meisen.dissertation.impl.parser.query.select.evaluator.TestGroupEvaluator;
import net.meisen.dissertation.impl.parser.query.select.group.TestGroupExpression;
import net.meisen.dissertation.impl.persistence.TestZipPersistor;
import net.meisen.dissertation.impl.time.granularity.TestTimeGranularityFactory;
import net.meisen.dissertation.impl.time.mapper.TestDateMapper;
import net.meisen.dissertation.impl.time.mapper.TestMapperFactory;
import net.meisen.dissertation.model.auth.permissions.TestDefinedPermission;
import net.meisen.dissertation.model.data.TestDataModel;
import net.meisen.dissertation.model.data.TestDataStructure;
import net.meisen.dissertation.model.data.TestMetaDataModel;
import net.meisen.dissertation.model.dataretriever.TestDataCollection;
import net.meisen.dissertation.model.dataretriever.TestDataRecord;
import net.meisen.dissertation.model.datasets.TestMultipleDataSetIterator;
import net.meisen.dissertation.model.descriptors.TestDescriptorModel;
import net.meisen.dissertation.model.descriptors.TestDescriptorPrimitiveDataType;
import net.meisen.dissertation.model.dimensions.TestDescriptorDimension;
import net.meisen.dissertation.model.dimensions.TestDimensionHandler;
import net.meisen.dissertation.model.dimensions.graph.TestDescriptorGraph;
import net.meisen.dissertation.model.dimensions.graph.TestTimeGraph;
import net.meisen.dissertation.model.dimensions.graph.TestTimeGraphMemberIndex;
import net.meisen.dissertation.model.dimensions.templates.TestDays;
import net.meisen.dissertation.model.dimensions.templates.TestMinutes;
import net.meisen.dissertation.model.dimensions.templates.TestTimeLevelTemplateManager;
import net.meisen.dissertation.model.handler.TestTidaModelHandler;
import net.meisen.dissertation.model.handler.TestTidaModelHandlerPersistency;
import net.meisen.dissertation.model.indexes.TestIndexKeyDefinition;
import net.meisen.dissertation.model.indexes.datarecord.TestIntervalIndex;
import net.meisen.dissertation.model.indexes.datarecord.TestMetaIndex;
import net.meisen.dissertation.model.indexes.datarecord.TestMetaIndexDimension;
import net.meisen.dissertation.model.indexes.datarecord.TestProcessedDataRecord;
import net.meisen.dissertation.model.indexes.datarecord.TestTidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.TestFactDescriptorModelSet;
import net.meisen.dissertation.model.indexes.datarecord.slices.TestFactDescriptorSet;
import net.meisen.dissertation.model.persistence.TestBasePersistor;
import net.meisen.dissertation.model.persistence.TestGroup;
import net.meisen.dissertation.model.persistence.TestIdentifier;
import net.meisen.dissertation.model.time.TestDateNormalizer;
import net.meisen.dissertation.model.time.granularity.TestDateBasedHelper;
import net.meisen.dissertation.model.time.granularity.TestDateFormat;
import net.meisen.dissertation.model.time.granularity.TestMinute;
import net.meisen.dissertation.model.time.mapper.TestBaseMapperFactory;
import net.meisen.dissertation.model.time.timeline.TestTimelineDefinition;
import net.meisen.dissertation.server.TestAuthServlet;
import net.meisen.dissertation.server.TestCommunication;
import net.meisen.dissertation.server.TestTidaServer;
import net.meisen.dissertation.server.session.TestSession;
import net.meisen.dissertation.server.session.TestSessionManager;

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
		TestContinuousIntIndexedCollection.class,

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
		TestMapperFactory.TestMapperFactorySuite.class,

		// Test the dataRetrievers
		TestDataCollection.class,
		TestDataRecord.class,
		TestPreProcessedDataRecord.class,
		TestDbDataRetriever.class,
		TestCsvDataRetriever.class,
		TestFixedStructureDataRetriever.class,

		// Test the preProcessors
		TestScriptPreProcessor.class,

		// Test the dataSets
		TestSingleStaticDataSet.class,
		TestDataRetrieverDataSet.class,
		TestMultipleDataSetIterator.class,

		// Test the different created instances
		TestDataStructure.class,
		TestMetaDataModel.class,
		TestDataModel.class,

		// Test the xsd and xslt
		TestXsdTidaModel.class,
		TestXsltTidaModel.class,

		// Test some bitmap implementations
		TestBitmapId.class,
		TestEWAHBitmap.class,
		TestRoaringBitmap.class,

		// Test the handler
		TestTidaModelHandler.class,

		// Test the record pre-processing
		TestProcessedDataRecord.class,

		// Test the caches
		TestMemoryMetaDataCache.class,
		TestFileMetaDataCache.class,
		TestMemoryIdentifierCache.class,
		TestFileIdentifierCache.class,
		TestFileBitmapIdCacheConfig.class,
		TestRecentlyUsedCachingStrategy.class,
		TestFileBitmapCache.class,
		TestFileFactDescriptorModelSetCache.class,
		TestFileCaches.TestFileCachesSuite.class,
		TestMapDbBitmapCache.class,
		TestMapDbDataRecordCache.class,
		TestIdsOnlyDataRecordCache.class,

		// Test the index for facts
		TestFactDescriptorSet.class,
		TestFactDescriptorModelSet.class,

		// Test the tida-indexes
		TestMetaIndexDimension.class,
		TestMetaIndex.class,
		TestIntervalIndex.class,
		TestDataRecordIndex.class,
		TestTidaIndex.class,

		// Test the fact-holder
		TestMapFactsArrayBased.class,
		TestMapFactsDescriptorBased.class,

		// Test the query
		TestDescriptorValue.class, TestDescriptorComperator.class,
		TestGroupExpression.class, TestDescriptorLogicEvaluator.class,
		TestGroupEvaluator.class, TestAliveQueries.class,
		TestLoadQueries.class,
		TestUnloadQueries.class,
		TestSelectQueries.class,
		TestInsertQueries.class,
		TestAddQueries.class,
		TestDropQueries.class,
		TestModifyQueries.class,
		TestGrantQueries.class,
		TestRevokeQueries.class,
		TestAssignQueries.class,
		TestRemoveQueries.class,
		TestGetQueries.class,
		TestDeleteQueries.class,

		// Test authentication manager
		TestDefinedPermission.class,
		TestMapDbAuthorizingRealm.class,
		TestShiroAuthManager.class,

		// Tests dimensions
		TestDescriptorDimension.class, TestDescriptorGraph.class,
		TestDays.class, TestMinutes.class,
		TestTimeLevelTemplateManager.TestTimeLevelTemplateManagerSuite.class,
		TestTimeGraph.class, TestTimeGraphMemberIndex.class,
		TestDimensionHandler.class,

		// Test the session management
		TestSession.class, TestSessionManager.TestSessionManagerSuite.class,

		// Test the server
		TestTidaServer.class, TestCommunication.TestCommunicationSuite.class,
		TestAuthServlet.class,

		// Tests re-running the server
		TestTidaModelHandlerPersistency.class })
public class AllUnitTests {

}
