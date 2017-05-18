package net.meisen.master.meike.performance;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.impl.parser.query.QueryFactory;
import net.meisen.dissertation.impl.parser.query.select.SelectResultRecords;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.datasets.DatasetFactory;
import net.meisen.master.meike.impl.logging.PerformanceLogger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Base class for the performance tests to provide some utility methods.
 */
public class BasePerformanceTest extends LoaderBasedTest {
    @Autowired
    @Qualifier(DefaultValues.QUERYFACTORY_ID)
    private QueryFactory queryFactory;

    protected final PerformanceLogger logger = new PerformanceLogger();

    @SuppressWarnings("unchecked")
    private <T extends IQuery> T createQuery(final String query) {
        return (T) queryFactory.parseQuery(query);
    }

    @SuppressWarnings("unchecked")
    private SelectResultRecords evaluateQuery(final TidaModel model,
                                              final String query) {
        final TidaIndex index = model.getIndex();

        final SelectResultRecords records =
                queryFactory.evaluateQuery(createQuery(query), null);
        records.determineResult(model);
        return records;
    }

    protected Dataset getDatasetFor(final String query, final TidaModel model,
                                    final DatasetFactory datasetFactory,
                                    final String datasetId) {
        final SelectResultRecords records = this.evaluateQuery(model, query);
        return datasetFactory.convertRecords(records, datasetId);
    }
}
