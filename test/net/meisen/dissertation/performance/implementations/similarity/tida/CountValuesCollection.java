package net.meisen.dissertation.performance.implementations.similarity.tida;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;

import net.meisen.dissertation.impl.time.series.TimeSeries;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;

import gnu.trove.impl.Constants;
import gnu.trove.map.hash.TIntIntHashMap;

public class CountValuesCollection {
	private static final int defaultValue = 0;
	private final Map<String, TIntIntHashMap> countResults;
	private final int size;

	public CountValuesCollection() {
		this(Constants.DEFAULT_CAPACITY);
	}

	public CountValuesCollection(final int size) {
		this.countResults = new HashMap<String, TIntIntHashMap>();
		this.size = size;
	}

	public void set(final String groupId, final int timepoint,
			final Bitmap bitmap) {
		set(groupId, timepoint, calcCount(bitmap));
	}

	public int set(final String groupId, final int timepoint, final int count) {
		TIntIntHashMap countResult = countResults.get(groupId);
		if (countResult == null) {
			countResult = new TIntIntHashMap(size,
					Constants.DEFAULT_LOAD_FACTOR, 0, defaultValue);
			countResults.put(groupId, countResult);
		}

		return countResult.put(timepoint, count);
	}

	public int get(final String groupId, final int timepoint) {
		final TIntIntHashMap countResult = countResults.get(groupId);
		if (countResult == null) {
			return defaultValue;
		} else {
			return countResult.get(timepoint);
		}
	}

	public boolean contains(final String groupId, final int timepoint) {
		final TIntIntHashMap countResult = countResults.get(groupId);
		if (countResult == null) {
			return false;
		} else {
			return countResult.containsKey(timepoint);
		}
	}

	public int calcCount(final Bitmap bitmap) {
		return bitmap == null ? 0 : bitmap.determineCardinality();
	}

	@Override
	public String toString() {
		final String nl = System.getProperty("line.separator");
		final StringBuilder sb = new StringBuilder();

		for (final Entry<String, TIntIntHashMap> e : countResults.entrySet()) {

			// add the id of the timeseries
			sb.append(e.getKey());
			sb.append(": ");

			// add each label
			final TIntIntHashMap vals = e.getValue();
			final int[] keys = vals.keys();
			Arrays.sort(keys);
			for (int i = 0; i < keys.length; i++) {
				if (i > 0) {
					sb.append("; ");
				}

				sb.append(String.format(Locale.US, "%d (%d)",
						vals.get(keys[i]), i));
			}
			sb.append(nl);
		}

		return sb.toString().trim();
	}
}
