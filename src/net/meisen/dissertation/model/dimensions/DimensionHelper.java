package net.meisen.dissertation.model.dimensions;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.meisen.dissertation.model.dimensions.graph.IDimensionGraph;

/**
 * Helper methods when working with {@code IDimension} and
 * {@code IDimensionGraph} instances.
 * 
 * @author pmeisen
 * 
 */
public class DimensionHelper {

	/**
	 * Gets the {@code Dimension} instances from the specified {@code graphs}.
	 * 
	 * @param graphs
	 *            the {@code DimensionGraph} instances to get the
	 *            {@code Dimension}'s from
	 * 
	 * @return the dimensions of the specified {@code graphs}
	 * 
	 * @see IDimension
	 * @see IDimensionGraph
	 */
	public static Map<String, IDimension> getDimensions(
			final Map<String, IDimensionGraph> graphs) {
		final Map<String, IDimension> dims = new HashMap<String, IDimension>();

		// get all the dimensions
		for (final Entry<String, IDimensionGraph> entry : graphs.entrySet()) {
			dims.put(entry.getKey(), entry.getValue().getDimension());
		}

		return dims;
	}
}
