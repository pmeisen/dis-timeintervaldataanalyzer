package net.meisen.master.meike.correctness;

import com.google.common.collect.ImmutableList;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.datasets.DatasetFactory;
import net.meisen.master.meike.impl.distances.intervals.EndDistance;
import net.meisen.master.meike.impl.distances.intervals.GapDistance;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.IntersectionDistance;
import net.meisen.master.meike.impl.distances.intervals.LengthDistance;
import net.meisen.master.meike.impl.distances.intervals.StartDistance;
import net.meisen.master.meike.impl.distances.intervals.WeightedSumDistance;
import net.meisen.master.meike.performance.BasePerformanceTest;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class SaschaBasedTest extends BasePerformanceTest {
    private static final String originalDate = "01.01.2017";
    protected static final ImmutableList<ImmutableList<String>> allCandidateDates = ImmutableList.of(
            ImmutableList.of(
                    "12.01.2017", "18.01.2017", "27.01.2017", "07.02.2017", "10.02.2017",
                    "23.02.2017", "27.02.2017", "11.03.2017", "27.03.2017", "04.04.2017",
                    "11.04.2017", "17.04.2017", "24.04.2017", "06.05.2017"),
            ImmutableList.of(
                    "12.01.2017", "18.01.2017", "27.01.2017", "07.02.2017", "10.02.2017",
                    "23.02.2017", "27.02.2017", "11.03.2017", "27.03.2017", "11.04.2017",
                    "17.04.2017", "24.04.2017", "06.05.2017", "21.05.2017", "08.06.2017",
                    "17.06.2017", "29.06.2017", "07.07.2017", "16.07.2017", "27.07.2017",
                    "05.08.2017", "19.08.2017", "10.09.2017", "01.10.2017", "09.10.2017",
                    "25.10.2017", "09.11.2017", "30.11.2017", "13.12.2017"),
            ImmutableList.of(
                    "10.01.2017", "29.01.2017", "17.02.2017", "28.02.2017", "15.03.2017",
                    "22.03.2017", "05.04.2017", "14.04.2017", "25.04.2017", "06.05.2017",
                    "10.05.2017", "22.05.2017", "01.06.2017", "08.06.2017", "15.06.2017",
                    "27.06.2017", "18.07.2017", "22.07.2017", "04.08.2017", "08.08.2017",
                    "14.08.2017", "28.09.2017", "16.10.2017", "21.10.2017", "08.11.2017",
                    "15.11.2017", "23.11.2017", "04.12.2017", "11.12.2017"),
            ImmutableList.of(
                    "07.01.2017", "22.01.2017", "09.02.2017", "20.02.2017", "09.03.2017",
                    "15.03.2017", "26.03.2017", "11.04.2017", "16.04.2017", "07.05.2017",
                    "15.05.2017", "24.05.2017", "01.06.2017", "10.06.2017", "23.06.2017",
                    "08.07.2017", "15.07.2017", "01.08.2017", "05.08.2017", "13.08.2017",
                    "25.08.2017", "12.09.2017", "21.09.2017", "04.10.2017", "13.10.2017",
                    "27.10.2017", "12.11.2017", "18.11.2017", "16.12.2017"),
            ImmutableList.of(
                    "08.01.2017", "23.01.2017", "13.02.2017", "06.03.2017", "15.03.2017",
                    "30.03.2017", "05.04.2017", "11.04.2017", "21.04.2017", "29.04.2017",
                    "12.05.2017", "19.05.2017", "28.05.2017", "02.06.2017", "10.06.2017",
                    "19.06.2017", "12.07.2017", "21.07.2017", "02.08.2017", "09.08.2017",
                    "15.08.2017", "05.09.2017", "14.09.2017", "23.09.2017", "04.10.2017",
                    "21.10.2017", "10.11.2017", "24.11.2017", "06.12.2017"),
            ImmutableList.of(
                    "13.01.2017", "21.01.2017", "06.02.2017", "15.02.2017", "24.02.2017",
                    "07.03.2017", "02.04.2017", "19.04.2017", "12.05.2017", "17.05.2017",
                    "25.05.2017", "04.06.2017", "14.06.2017", "27.06.2017", "03.07.2017",
                    "13.07.2017", "30.07.2017", "14.08.2017", "25.08.2017", "04.09.2017",
                    "10.09.2017", "23.09.2017", "14.10.2017", "23.10.2017", "27.10.2017",
                    "05.11.2017", "16.11.2017", "01.12.2017", "11.12.2017"),
            ImmutableList.of(
                    "10.01.2017", "12.01.2017", "28.01.2017", "03.02.2017", "15.02.2017",
                    "24.02.2017", "28.02.2017", "07.03.2017", "15.03.2017", "23.03.2017",
                    "01.04.2017", "21.04.2017", "26.04.2017", "05.05.2017", "11.05.2017",
                    "31.05.2017", "17.06.2017", "20.06.2017", "01.07.2017", "10.07.2017",
                    "25.07.2017", "16.08.2017", "07.09.2017", "15.09.2017", "28.09.2017",
                    "04.10.2017", "17.11.2017", "09.12.2017", "16.12.2017"),
            ImmutableList.of(
                    "11.01.2017", "20.01.2017", "04.02.2017", "11.02.2017", "14.02.2017",
                    "25.02.2017", "14.03.2017", "21.03.2017", "07.04.2017", "16.04.2017",
                    "25.04.2017", "11.05.2017", "18.05.2017", "21.05.2017", "06.06.2017",
                    "14.06.2017", "20.06.2017", "10.07.2017", "19.07.2017", "26.07.2017",
                    "15.08.2017", "24.08.2017", "10.09.2017", "21.09.2017", "07.10.2017",
                    "15.10.2017", "24.11.2017", "30.11.2017", "16.12.2017"));

    private TidaModel loadModel(final int modelNumber) {
        return m("/net/meisen/master/meike/correctness/data/sascha" + modelNumber + ".xml", true);
    }

    private Dataset getForDate(final String date, final TidaModel model,
                               final DatasetFactory datasetFactory,
                               final int modelNumber) {
        final String query = "SELECT RECORDS FROM sascha" + modelNumber
                + " DURING [" + date + " 00:00:00, " + date + " 23:59:59]";
        return this.getDatasetFor(query, model, datasetFactory, date);
    }

    private static IIntervalDistance createIntervalDistance(final List<Double> weights) {
        assert 5 == weights.size();

        final Map<IIntervalDistance, Double> distances = new HashMap<>();
        distances.put(new EndDistance(), weights.get(0));
        distances.put(new GapDistance(), weights.get(1));
        distances.put(new IntersectionDistance(), weights.get(2));
        distances.put(new LengthDistance(), weights.get(3));
        distances.put(new StartDistance(), weights.get(4));
        return new WeightedSumDistance(distances);
    }

    protected Datasets loadDatasets(final int modelNumber, final List<String> candidateDates) {
        final TidaModel model = this.loadModel(modelNumber);
        final DatasetFactory datasetFactory = DatasetFactory.forModel(model);

        final Dataset original = this.getForDate(originalDate, model, datasetFactory, modelNumber);
        final List<Dataset> candidates = new ArrayList<>();
        for (final String candidateDate : candidateDates) {
            candidates.add(this.getForDate(candidateDate, model, datasetFactory, modelNumber));
        }
        return new Datasets(original, candidates);
    }
}
