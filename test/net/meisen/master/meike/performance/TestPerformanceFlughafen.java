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
import net.meisen.master.meike.impl.distances.datasets.IDatasetDistance;
import net.meisen.master.meike.impl.distances.datasets.PlainDistance;
import net.meisen.master.meike.impl.distances.intervals.EndDistance;
import net.meisen.master.meike.impl.distances.intervals.GapDistance;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.IntersectionDistance;
import net.meisen.master.meike.impl.distances.intervals.LengthDistance;
import net.meisen.master.meike.impl.distances.intervals.StartDistance;
import net.meisen.master.meike.impl.distances.intervals.WeightedSumDistance;
import net.meisen.master.meike.impl.matching.IDatasetMinCostMatcher;
import net.meisen.master.meike.impl.matching.hungarian.KuhnMunkres;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.assertEquals;

/**
 * Performance tests using the flughafen data.
 */
public class TestPerformanceFlughafen extends LoaderBasedTest {
    @Autowired
    @Qualifier(DefaultValues.QUERYFACTORY_ID)
    private QueryFactory queryFactory;

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

    private Dataset getDatasetForDate(final String date, final TidaModel model,
                                      final DatasetFactory datasetFactory) {
        final SelectResultRecords records = this.evaluateQuery(model,
                "SELECT RECORDS FROM flughafen WITHIN [" + date + " 00:00:00, " + date + " 23:59:59]");

        return datasetFactory.convertRecords(records);
    }

    private IDatasetMinCostMatcher createKuhnMunkresMatcher() {
        final Map<IIntervalDistance, Double> distances = new HashMap<>();
        distances.put(new EndDistance(), 1.0);
        distances.put(new GapDistance(), 1.0);
        distances.put(new IntersectionDistance(), 1.0);
        distances.put(new LengthDistance(), 1.0);
        distances.put(new StartDistance(), 1.0);
        return KuhnMunkres.from(new WeightedSumDistance(distances));
    }

    @Test
    public void testPlainDistanceKuhnMunkres() {
        final TidaModel model = m("/net/meisen/master/meike/performance/flughafen-model.xml", true);
        final DatasetFactory datasetFactory = DatasetFactory.forModel(model);

        final long loadStartTime = System.currentTimeMillis();

        final Dataset dayOne = this.getDatasetForDate("05.01.2008", model, datasetFactory);
        final Dataset dayTwo = this.getDatasetForDate("02.01.2008", model, datasetFactory);
        final Dataset dayThree = this.getDatasetForDate("03.01.2008", model, datasetFactory);
        final Dataset dayFour = this.getDatasetForDate("04.01.2008", model, datasetFactory);

        final long loadTime = System.currentTimeMillis() - loadStartTime;
        System.out.println("Loading took " + loadTime + " milliseconds.");

        assertEquals(2318, dayOne.getIntervals().size());
        assertEquals(2521, dayTwo.getIntervals().size());
        assertEquals(2618, dayThree.getIntervals().size());
        assertEquals(2501, dayFour.getIntervals().size());

        final IDatasetDistance distance =
                PlainDistance.from(this.createKuhnMunkresMatcher());

        final long distanceStartTime = System.currentTimeMillis();

        final double cost1 = distance.calculate(dayOne, dayTwo);
        final double cost2 = distance.calculate(dayOne, dayThree);
        final double cost3 = distance.calculate(dayOne, dayFour);

        final long distanceTime = System.currentTimeMillis() - distanceStartTime;
        System.out.println("Calculating the distances took " + distanceTime + " milliseconds.");

        assertEquals(9238.180531932865, cost1, 0.0000001);
        assertEquals(9148.662843561984, cost2, 0.0000001);
        assertEquals(8968.869846094394, cost3, 0.0000001);
    }
}
