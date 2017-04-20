package net.meisen.master.meike.impl.distances.datasets;

import net.meisen.dissertation.impl.parser.query.select.SelectResultRecords;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.master.meike.impl.distances.intervals.Interval;

import java.util.Arrays;
import java.util.Date;
import java.util.Set;
import java.util.stream.Collectors;

import static java.lang.Math.max;

/**
 * Factory for creating {@link Dataset}s from records.
 */
public class DatasetFactory {
    private final TidaModel model;

    private int startPos = -1;
    private int endPos = -1;

    private DatasetFactory(final TidaModel model) {
        this.model = model;
    }

    /**
     * Initializes a factory for creating datasets for the given model.
     *
     * @param model The tida model to use; must not be {@code null}.
     * @return an initialized dataset factory
     */
    public static DatasetFactory forModel(final TidaModel model) {
        assert null != model;

        return new DatasetFactory(model);
    }

    /**
     * Converts the given select result records to a dataset of {@link Interval}s.
     *
     * @param records The results from a record select query; must not be {@code null}.
     * @return a corresponding dataset.
     */
    public Dataset convertRecords(final SelectResultRecords records) {
        assert null != records;

        final long offset = null == records.getQuery().getInterval()
                ? 0
                : ((Date) records.getQuery().getInterval().getStart()).getTime();

        final TidaIndex index = this.model.getIndex();
        final Set<Interval> intervals =
                Arrays.stream(records.getSelectedRecords().getIds())
                        .mapToObj(id -> this.getInterval(index, id, offset))
                        .collect(Collectors.toSet());

        return new Dataset(intervals);
    }

    protected Interval getInterval(final TidaIndex index, final int id, final long timeOffset) {
        final Object[] values = index.getRecordAsArray(id);

        // the [START] and [END] is always located at pos 1 and 2, 0 is [ID]
        final long startTime = max(0, ((Date) values[1]).getTime() - timeOffset);
        final long endTime = max(0, ((Date) values[2]).getTime() - timeOffset);
        return new Interval(0, 0);
    }
}
