package net.meisen.master.meike.correctness;

import javafx.util.Pair;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.mapping.Mapping;
import net.meisen.master.meike.impl.mapping.costCalculation.ICostCalculator;

class KuhnMunkresResult {
    final Dataset dataset;
    final Pair<Mapping, Double> plainDistanceResult;
    final Pair<Mapping, Double> iterativeShiftDistanceResult;
    final Pair<Mapping, Double> bestShiftDistanceResult;

    private KuhnMunkresResult(final Dataset dataset,
                              final Pair<Mapping, Double> plainDistanceResult,
                              final Pair<Mapping, Double> iterativeShiftDistanceResult,
                              final Pair<Mapping, Double> bestShiftDistanceResult) {
        this.dataset = dataset;
        this.plainDistanceResult = plainDistanceResult;
        this.iterativeShiftDistanceResult = iterativeShiftDistanceResult;
        this.bestShiftDistanceResult = bestShiftDistanceResult;
    }

    static class Builder {
        private final Dataset dataset;
        private Mapping plainDistanceResult;
        private Mapping iterativeShiftDistanceResult;
        private Mapping bestShiftDistanceResult;

        private Builder(final Dataset dataset) {
            this.dataset = dataset;
        }

        public static Builder forDataset(final Dataset dataset) {
            assert null != dataset;

            return new Builder(dataset);
        }

        public KuhnMunkresResult build(final ICostCalculator costCalculator) {
            return new KuhnMunkresResult(this.dataset,
                    new Pair<>(this.plainDistanceResult, costCalculator.calculateCost(this.plainDistanceResult)),
                    new Pair<>(this.iterativeShiftDistanceResult, costCalculator.calculateCost(this.iterativeShiftDistanceResult)),
                    new Pair<>(this.bestShiftDistanceResult, costCalculator.calculateCost(this.bestShiftDistanceResult)));
        }

        public Builder withPlainDistanceResult(final Mapping plainDistanceResult) {
            this.plainDistanceResult = plainDistanceResult;
            return this;
        }

        public Builder withIterativeShiftDistanceResult(final Mapping iterativeShiftDistanceResult) {
            this.iterativeShiftDistanceResult = iterativeShiftDistanceResult;
            return this;
        }

        public Builder withBestShiftDistanceResult(final Mapping bestShiftDistanceResult) {
            this.bestShiftDistanceResult = bestShiftDistanceResult;
            return this;
        }
    }

    @Override
    public String toString() {
        return this.dataset.getId() +
                " - Plain: " + plainDistanceResult.getValue() +
                "\t Iterative: " + iterativeShiftDistanceResult.getValue() +
                " (" + (iterativeShiftDistanceResult.getKey().getOffset() / 60000) + ")" +
                "\t Best shift: " + bestShiftDistanceResult.getValue() +
                " (" + (bestShiftDistanceResult.getKey().getOffset() / 60000) + ")";
    }

    public String getImprovements() {
        return String.format("%4.3f" , iterativeShiftDistanceResult.getValue()  - plainDistanceResult.getValue()) + " iterative,\n" +
                String.format("%4.3f", bestShiftDistanceResult.getValue() - plainDistanceResult.getValue()) + " best";
    }
}
