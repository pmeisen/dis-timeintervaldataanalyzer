package net.meisen.dissertation.performance.implementations.similarity;

import java.util.List;
import java.util.Set;

/**
 * Different {@code DistanceType} implementations.
 * 
 * @author pmeisen
 * 
 */
public interface DistanceType {

	/**
	 * The Euclidian distance.
	 */
	public final DistanceType EUCLID = new DistanceType() {

		@Override
		public double apply(final Set<List<Object>> labels,
				final EventTable et1, final EventTable et2) {
			double distance = 0.0;

			for (final List<Object> label : labels) {
				for (int i = 0; i < et1.getTimeSize(); i++) {
					final double val = et1.get(i, label);
					final double cmpVal = et2.get(i, label);

					if (cmpVal == Double.NEGATIVE_INFINITY
							|| val == Double.NEGATIVE_INFINITY) {
						return Double.NEGATIVE_INFINITY;
					} else {
						distance += Math.pow(cmpVal - val, 2);
					}
				}
			}

			return Math.sqrt(distance);
		}
	};

	/**
	 * The Manhattan distance.
	 */
	public final DistanceType MANHATTAN = new DistanceType() {

		@Override
		public double apply(final Set<List<Object>> labels,
				final EventTable et1, final EventTable et2) {
			double distance = 0.0;

			for (final List<Object> label : labels) {
				for (int i = 0; i < et1.getTimeSize(); i++) {
					final double val = et1.get(i, label);
					final double cmpVal = et2.get(i, label);

					if (cmpVal == Double.NEGATIVE_INFINITY
							|| val == Double.NEGATIVE_INFINITY) {
						return Double.NEGATIVE_INFINITY;
					} else {
						distance += Math.abs(cmpVal - val);
					}
				}
			}

			return distance;
		}
	};

	/**
	 * Method used to apply the distance to be calculated for the two
	 * {@code EventTable} instances.
	 * 
	 * @param labels
	 *            the labels to be considered
	 * @param et1
	 *            the {@code EventTable} to compare with
	 * @param et2
	 *            the {@code EventTable} to compare with
	 *            
	 * @return the distance between the two tables
	 */
	public double apply(final Set<List<Object>> labels, final EventTable et1,
			final EventTable et2);
}
