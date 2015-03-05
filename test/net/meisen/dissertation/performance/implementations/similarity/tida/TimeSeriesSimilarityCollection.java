package net.meisen.dissertation.performance.implementations.similarity.tida;

import net.meisen.dissertation.impl.time.series.TimeSeriesCollection;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;

public class TimeSeriesSimilarityCollection extends TimeSeriesCollection
		implements Comparable<TimeSeriesSimilarityCollection> {

	private final CountValuesCollection countValues;
	private final StructureCollection structureValues;

	private double measureDistance;
	private double countDistance;
	private double structureDistance;

	public TimeSeriesSimilarityCollection(final int measureSize,
			final int countSize, final Class<?> type,
			final BaseIndexFactory factory) {
		super(measureSize, type, factory);

		countValues = new CountValuesCollection(countSize);
		structureValues = new StructureCollection(countSize);

		measureDistance = 0.0;
		countDistance = 0.0;
		structureDistance = 0.0;
	}

	public void setCountDistance(final double distance) {
		this.countDistance = distance;
	}

	public double getCountDistance() {
		return countDistance;
	}

	public double getStructureDistance() {
		return structureDistance;
	}

	public void setStructureDistance(final double structureDistance) {
		this.structureDistance = structureDistance;
	}

	public void setMeasureDistance(final double distance) {
		this.measureDistance = distance;
	}

	public double getMeasureDistance() {
		return measureDistance;
	}

	public double getTotalDistance() {
		return measureDistance + countDistance + structureDistance;
	}

	@Override
	public boolean equals(final Object o) {
		if (o == this) {
			return true;
		} else if (o instanceof TimeSeriesSimilarityCollection) {
			return ((TimeSeriesSimilarityCollection) o).measureDistance == measureDistance
					&& ((TimeSeriesSimilarityCollection) o).countDistance == countDistance
					&& ((TimeSeriesSimilarityCollection) o).structureDistance == structureDistance
					&& super.equals(o);
		} else {
			return false;
		}
	}

	@Override
	public int hashCode() {
		final long bits = Double.doubleToLongBits(getTotalDistance());
		return (int) (bits ^ (bits >>> 32));
	}

	@Override
	public int compareTo(final TimeSeriesSimilarityCollection tssc) {

		/*
		 * A negative integer, zero, or a positive integer as this object is
		 * less than, equal to, or greater than the specified object.
		 */
		final double totalDistance = getTotalDistance();
		final double cmpDistance = tssc.getTotalDistance();
		if (totalDistance < cmpDistance) {
			return -1;
		} else if (totalDistance > cmpDistance) {
			return 1;
		} else {
			return 0;
		}
	}

	public double setMeasure(final int pos, final String timeSeriesId,
			final double value) {
		return setValue(pos, timeSeriesId, value);
	}

	public int setCount(final String groupId, final int pos, final int count) {
		return countValues.set(groupId, pos, count);
	}

	public int[] setStructure(final String groupId, final int pos,
			final Bitmap bitmap) {
		return structureValues.set(groupId, pos, bitmap);
	}

	public int[] getStructure(final String groupId, final int timepoint) {
		return structureValues.get(groupId, timepoint);
	}

	public boolean isProcessed(final String groupId, final int timepoint) {
		return countValues.contains(groupId, timepoint);
	}

	@Override
	public String toString() {
		final String nl = System.getProperty("line.separator");
		final StringBuilder sb = new StringBuilder();

		sb.append("Distance: " + getTotalDistance());
		sb.append(nl);

		sb.append("Measure (" + getMeasureDistance() + "):");
		sb.append(nl);
		sb.append(super.toString());

		sb.append(nl);

		sb.append("Count: (" + getCountDistance() + "):");
		sb.append(nl);
		sb.append(countValues.toString());

		sb.append(nl);

		sb.append("Structure: (" + getStructureDistance() + "):");
		sb.append(nl);
		sb.append(structureValues.toString());

		return sb.toString();
	}
	
	public void finish() {
		structureValues.finish();
	}
}
