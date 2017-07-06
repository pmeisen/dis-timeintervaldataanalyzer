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
    public static String getPlotterCommand(final int modelNumber, final String date, final Mapping mapping, final String suffix) {
        final String american_date = "2017-" + date.substring(3, 5) + "-" + date.substring(0,2);
        return "python specific-plotter.py /home/meike/masterarbeit/data/sascha/sascha"
                + modelNumber + ".csv "
                + american_date + " "
                + mapping.getOffset()/1000 + " "
                + mapping.getMappingIndicesString() + " "
                + modelNumber + "-" + american_date + suffix;
    }

    public static String getRepresentation(final Dataset dataset) {
        return String.join(", ", getIntervalRepresentations(dataset));
    }

    private static List<String> getIntervalRepresentations(final Dataset dataset) {
        return dataset.getIntervals().stream()
                .map(Interval::toString)
                .collect(Collectors.toList());
    }
}
