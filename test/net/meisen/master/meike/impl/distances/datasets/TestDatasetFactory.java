package net.meisen.master.meike.impl.distances.datasets;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.impl.parser.query.QueryFactory;
import net.meisen.dissertation.impl.parser.query.select.SelectResultRecords;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.parser.query.IQuery;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import static org.junit.Assert.assertEquals;

/**
 * Tests for the {@link DatasetFactory}.
 */
public class TestDatasetFactory extends LoaderBasedTest {

    private final String modelXmlPath =
            "/net/meisen/master/meike/impl/distances/plainModel.xml";

    @Autowired
    @Qualifier(DefaultValues.QUERYFACTORY_ID)
    private QueryFactory queryFactory;

    /**
     * Creates the queryFactory's result for the {@code query}.
     *
     * @param query
     *            the query to be created
     * @return the created instance of an {@link IQuery} implementation
     */
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

    @Test
    public void testConversionFromRecords() {
        final TidaModel model = m(this.modelXmlPath);
        final DatasetFactory factory = DatasetFactory.forModel(model);

        final SelectResultRecords records =
                this.evaluateQuery(model, "SELECT RECORDS FROM plainModel");
        final Dataset result = factory.convertRecords(records);

        assertEquals(6, result.getIntervals().size());
    }
}
