package net.meisen.master.meike.testUtils;

import net.meisen.master.meike.impl.distances.intervals.EndDistance;
import net.meisen.master.meike.impl.distances.intervals.GapDistance;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.IntersectionDistance;
import net.meisen.master.meike.impl.distances.intervals.LengthDistance;
import net.meisen.master.meike.impl.distances.intervals.StartDistance;
import net.meisen.master.meike.impl.distances.intervals.WeightedSumDistance;

import java.util.HashMap;
import java.util.Map;

/**
 * Utility class for creating objects needed for tests.
 */
public class Factories {
    public static IIntervalDistance weightedDistance(
            double startWeight, double endWeight, double lengthWeight,
            double gapWeight, double intersectionWeight) {
        final Map<IIntervalDistance, Double> distances = new HashMap<>();
        distances.put(new EndDistance(), endWeight);
        distances.put(new GapDistance(), gapWeight);
        distances.put(new IntersectionDistance(), intersectionWeight);
        distances.put(new LengthDistance(), lengthWeight);
        distances.put(new StartDistance(), startWeight);
        return new WeightedSumDistance(distances);
    }
}
