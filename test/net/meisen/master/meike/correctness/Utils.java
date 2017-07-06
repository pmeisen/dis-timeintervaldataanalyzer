package net.meisen.master.meike.correctness;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Utils to help with test reporting.
 */
public class Utils {
    public static String getSpecificPlotterCommand(final int modelNumber,
                                                   final String date,
                                                   final Mapping mapping,
                                                   final String suffix) {
        return "python specific-plotter.py /home/meike/masterarbeit/data/sascha/sascha"
                + modelNumber + ".csv "
                + getAmericanDate(date) + " "
                + mapping.getOffset()/1000 + " "
                + mapping.getMappingIndicesString() + " "
                + modelNumber + "-" + getAmericanDate(date) + suffix;
    }

    public static class Neighbor {
        public final String date;
        public final Mapping bestMapping;
        public final double cost;

        public Neighbor(final String date, final Mapping bestMapping, final double cost) {
            this.date = date;
            this.bestMapping = bestMapping;
            this.cost = cost;
        }
    }

    public static String getNeighborsPlotterCommand(final String inputFileName,
                                                    final String outputFileName,
                                                    final String title,
                                                    final List<Neighbor> neighbors) {
        final StringBuilder commandBuilder = new StringBuilder("python neighbors-plotter.py ");
        commandBuilder.append(inputFileName);
        commandBuilder.append(" \"");
        for (final Neighbor neighbor : neighbors) {
            if (neighbors.indexOf(neighbor) > 0) {
                commandBuilder.append("_");
            }
            commandBuilder.append(getAmericanDate(neighbor.date));
            commandBuilder.append(";");
            commandBuilder.append(neighbor.bestMapping.getOffset());
            commandBuilder.append(";");
            commandBuilder.append(neighbor.bestMapping.getMappingIndicesString());
            commandBuilder.append(";");
            commandBuilder.append(neighbor.cost);
        }
        commandBuilder.append("\" \"");
        commandBuilder.append(title);
        commandBuilder.append("\" ");
        commandBuilder.append(outputFileName);
        return commandBuilder.toString();
    }

    public static String getRepresentation(final Dataset dataset) {
        return String.join(", ", getIntervalRepresentations(dataset));
    }

    private static String getAmericanDate(final String germanDate) {
        return "2017-" + germanDate.substring(3, 5) + "-" + germanDate.substring(0,2);
    }

    private static List<String> getIntervalRepresentations(final Dataset dataset) {
        return dataset.getIntervals().stream()
                .map(Interval::toString)
                .collect(Collectors.toList());
    }
}
