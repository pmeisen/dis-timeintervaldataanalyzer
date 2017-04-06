package net.meisen.master.meike.impl.distances.intervals;

import net.meisen.dissertation.model.indexes.datarecord.ProcessedDataRecord;

/**
 * Allows calculating some distance value of two {@link ProcessedDataRecord}s
 * that depends only on the record's {@link Interval}.
 */
@FunctionalInterface
public interface IIntervalDistance {

    /**
     * Calculates the distance value for the two given intervals. Note that this
     * is not necessarily symmetric.
     *
     * @param original
     *            The original {@link Interval}
     * @param other
     *            Another {@link Interval} to be compared to the original
     * @return the distance value of the two intervals
     */
    double calculate(final Interval original, final Interval other);
}
