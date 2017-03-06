package net.meisen.dissertation.server;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.impl.indexes.TestDataRecordIndex;
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
import net.meisen.dissertation.jdbc.TestTidaConnection;
import net.meisen.dissertation.jdbc.TestTidaDatabaseMetaData;
import net.meisen.dissertation.jdbc.TestTidaDriver;
import net.meisen.dissertation.jdbc.TestTidaResultSet;
import net.meisen.dissertation.model.dimensions.TestDescriptorDimension;
import net.meisen.dissertation.model.dimensions.TestDimensionHandler;
import net.meisen.dissertation.model.dimensions.graph.TestDescriptorGraph;
import net.meisen.dissertation.model.dimensions.graph.TestTimeGraph;
import net.meisen.dissertation.model.dimensions.graph.TestTimeGraphMemberIndex;
import net.meisen.dissertation.model.dimensions.templates.TestDays;
import net.meisen.dissertation.model.dimensions.templates.TestMinutes;
import net.meisen.dissertation.model.dimensions.templates.TestTimeLevelTemplateManager;
import net.meisen.dissertation.model.handler.TestTidaModelHandler;
import net.meisen.dissertation.model.indexes.datarecord.TestIntervalIndex;
import net.meisen.dissertation.model.indexes.datarecord.TestMetaIndex;
import net.meisen.dissertation.model.indexes.datarecord.TestMetaIndexDimension;
import net.meisen.dissertation.model.indexes.datarecord.TestProcessedDataRecord;
import net.meisen.dissertation.model.indexes.datarecord.TestTidaIndex;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * All tests together as a {@link Suite}
 *
 * @author pmeisen
 */
@RunWith(Suite.class)
@Suite.SuiteClasses({

        // Test the configuration
        TestConfig.TestConfigSuite.class, // really long init, 41s

        // Test the handler
        TestTidaModelHandler.class,

        // Test the record pre-processing
        TestProcessedDataRecord.class,

        // Test the tida-indexes
        TestMetaIndexDimension.class,
        TestMetaIndex.class,
        TestIntervalIndex.class,
        TestDataRecordIndex.class,
        TestTidaIndex.class,

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

        // Tests dimensions
        TestDescriptorDimension.class, // long init, 6s
        TestDescriptorGraph.class, // long init, 7s
        TestDays.class,
        TestMinutes.class,
        TestTimeLevelTemplateManager.TestTimeLevelTemplateManagerSuite.class,
        TestTimeGraph.class, TestTimeGraphMemberIndex.class,
        TestDimensionHandler.class,

        // Test the server
        TestTidaServer.class, // 47s
        TestCommunication.TestCommunicationSuite.class, // 4m
        TestAuthServlet.class, // 23s

        // JDBC related tests
        TestTidaDriver.class,
        TestTidaConnection.class,
        TestTidaResultSet.class,
        TestTidaDatabaseMetaData.TestTidaDatabaseMetaDataSuite.class,
        net.meisen.dissertation.jdbc.protocol.TestCommunication.class
})
public class AllTravisTests {

}
