package net.meisen.master.meike.performance;

import com.google.common.collect.ImmutableList;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.datasets.DatasetFactory;
import org.junit.Test;

/**
 * Performance tests for the {@link DatasetFactory}; this is now mainly a test
 * for finding the reason for why accessing the interval boundaries is so slow.
 */
public class TestPerformanceDatasetFactory extends BasePerformanceTest {
    private TidaModel loadModel() {
        return m("/net/meisen/master/meike/performance/flughafen-model.xml", true);
    }

    private Dataset getDatasetForDate(final String date, final TidaModel model,
                                      final DatasetFactory datasetFactory) {
        final String query = "SELECT RECORDS FROM flughafen DURING [" + date + " 00:00:00, " + date + " 23:59:59]";
        return this.getDatasetFor(query, model, datasetFactory);
    }

    @Test
    public void testLoadingPerformance() {
        final TidaModel model = this.loadModel();
        final DatasetFactory datasetFactory = DatasetFactory.forModel(model);

        final ImmutableList<String> dates = ImmutableList.of("01.01.2008",
                "02.01.2008", "03.01.2008", "04.01.2008", "05.01.2008");

        for (int i = 1; i <= 3; i++) {
            this.logger.logTiming("Iteration " + i + " total", () -> {
                for (final String date : dates) {
                    this.logger.logTiming(date, () -> {
                        this.getDatasetForDate(date, model, datasetFactory);
                    });
                }
            });
        }
    }
}
