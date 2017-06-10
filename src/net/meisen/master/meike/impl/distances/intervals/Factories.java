package net.meisen.master.meike.impl.distances.intervals;

import java.util.HashMap;
import java.util.Map;

/**
 * Utility class for creating interval distances.
 */
public class Factories {
    public static IIntervalDistance weightedDistance(
            double startWeight, double endWeight, double lengthWeight,
            double gapWeight, double intersectionWeight) {
        final Map<IIntervalDistance, Double> distances = new HashMap<>();
        distances.put(new StartDistance(), startWeight);
        distances.put(new EndDistance(), endWeight);
        distances.put(new LengthDistance(), lengthWeight);
        distances.put(new GapDistance(), gapWeight);
        distances.put(new IntersectionDistance(), intersectionWeight);
        return new WeightedSumDistance(distances);
    }
}
