package net.meisen.master.meike.impl.distances.datasets;

import net.meisen.dissertation.impl.parser.query.select.SelectResultRecords;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.master.meike.impl.distances.intervals.Interval;

import java.util.Arrays;
import java.util.Date;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Factory for creating {@link Dataset}s from records.
 */
public class DatasetFactory {
    private final TidaModel model;

    private DatasetFactory(final TidaModel model) {
        this.model = model;
    }

    /**
     * Initializes a factory for creating datasets for the given model.
     *
     * @param model
     *            The tida model to use; must not be {@code null}.
     * @return an initialized dataset factory
     */
    public static DatasetFactory forModel(final TidaModel model) {
        assert null != model;

        return new DatasetFactory(model);
    }

    /**
     * Converts the given select result records to a dataset of {@link Interval}s.
     *
     * @param records
     *            The results from a record select query; must not be {@code null}.
     * @return a corresponding dataset.
     */
    public Dataset convertRecords(final SelectResultRecords records) {
        assert null != records;

        final TidaIndex index = this.model.getIndex();
        final Set<Interval> intervals =
                Arrays.stream(records.getSelectedRecords().getIds())
                .mapToObj(id -> {
                    final Object[] times = index.getTimePointValuesOfRecord(id);
                    final long startTime = ((Date) times[0]).getTime();
                    final long endTime = ((Date) times[1]).getTime();
                    return new Interval(startTime, endTime);
                }).collect(Collectors.toSet());

        return new Dataset(intervals);
    }
}
