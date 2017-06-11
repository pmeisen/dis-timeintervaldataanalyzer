package net.meisen.master.meike.impl.logging;

import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Allows logging mappings for getting an impression of how the solution evolved.
 */
public class MappingLogger implements ILogger {
    private int counter;
    private final int modelNumber;
    private final String date;
    private final String suffix;
    public boolean enabled = true;

    private MappingLogger(final int modelNumber, final String date, final String suffix) {
        this.counter = 0;
        this.modelNumber = modelNumber;
        this.date = date;
        this.suffix = suffix;
    }

    public static MappingLogger createFor(final int modelNumber, final String date, final String suffix) {
        return new MappingLogger(modelNumber, date, suffix);
    }

    public void log(final Mapping mapping) {
        log(this.getPlotterCommand(mapping, "-" + this.suffix + "-" + (++this.counter)));
    }

    public void log(final List<Long> offsets) {
        log("Offsets: " + String.join(", ",
                offsets.stream().map(cost -> String.format("%d", cost / 1000)).collect(Collectors.toList())));
    }

    @Override
    public void log(final String message) {
        if (this.enabled) {
            System.out.println(message);
        }
    }

    private String getPlotterCommand(final Mapping mapping, final String suff) {
        final String american_date = "2017-" + date.substring(3, 5) + "-" + date.substring(0,2);
        return "python specific-plotter.py /home/meike/masterarbeit/data/sascha/sascha"
                + modelNumber + ".csv "
                + american_date + " "
                + mapping.getOffset()/1000 + " "
                + mapping.getMappingIndices() + " "
                + modelNumber + "-" + american_date + suff;
    }
}
