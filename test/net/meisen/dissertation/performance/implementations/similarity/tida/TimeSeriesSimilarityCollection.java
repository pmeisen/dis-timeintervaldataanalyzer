package net.meisen.dissertation.performance.implementations.similarity.tida;

import net.meisen.dissertation.impl.time.series.TimeSeriesCollection;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;

public class TimeSeriesSimilarityCollection extends TimeSeriesCollection
		implements Comparable<TimeSeriesSimilarityCollection> {
	private double distance;

	public TimeSeriesSimilarityCollection(final int size, final Class<?> type,
			final BaseIndexFactory factory) {
		super(size, type, factory);

		distance = 0.0;
	}

	public void setDistance(final double distance) {
		this.distance = distance;
	}

	public double getDistance() {
		return distance;
	}

	@Override
	public int compareTo(final TimeSeriesSimilarityCollection tssc) {
		
		/*
		 * A negative integer, zero, or a positive integer as this object is
		 * less than, equal to, or greater than the specified object.
		 */
		final double cmpDistance = tssc.getDistance();
		if (distance < cmpDistance) {
			return -1;
		} else if (distance > cmpDistance) {
			return 1;
		} else {
			return 0;
		}
	}
}
