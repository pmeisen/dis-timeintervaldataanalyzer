package net.meisen.dissertation.impl.measures;

import gnu.trove.impl.Constants;
import gnu.trove.map.hash.TIntDoubleHashMap;

import java.util.Arrays;

public class MapFactsArrayBased {
	private final int maxRecordId;
	private final TIntDoubleHashMap map;

	private boolean synchronizedMaps;

	public MapFactsArrayBased(final int maxRecordId) {
		this(maxRecordId, -1);
	}

	public MapFactsArrayBased(final int maxRecordId, final int capacity) {
		if (maxRecordId < 0) {
			throw new IllegalArgumentException(
					"The maxRecordId must be larger or equal to 0.");
		}

		this.map = new TIntDoubleHashMap(
				capacity < 0 ? Constants.DEFAULT_CAPACITY : capacity,
				Constants.DEFAULT_LOAD_FACTOR, -1, Double.NaN);
		this.maxRecordId = maxRecordId;

		synchronizedMaps = false;
	}

	public void set(final int recordId, final double factValue) {
		if (maxRecordId < recordId) {
			throw new IllegalArgumentException(
					"The maximal record identifier can be '"
							+ maxRecordId
							+ "'. Tried to set a fact for a record with identifier '"
							+ recordId + "'.");
		} else if (recordId < 0) {
			throw new IllegalArgumentException(
					"The record cannot have an identifier smaller than 0.");
		}

		final double oldValue = map.put(recordId, factValue);

		// check if the maps are changed and have to be resorted
		if (synchronizedMaps) {
			synchronizedMaps = oldValue == factValue;
		}
	}

	public int size() {
		return map.size();
	}

	public double getFactOfRecord(final int recordId) {
		if (maxRecordId < recordId) {
			throw new IllegalArgumentException("The recordId '" + recordId
					+ "' is larger than the maxRecordId '" + maxRecordId + "'.");
		}

		// get the value stored in the map
		return map.get(recordId);
	}

	public int[] recordIds() {
		return map.keys();
	}

	public double[] facts() {
		return map.values();
	}

	public double[] sortedFacts() {
		final double[] facts = facts();
		Arrays.sort(facts);

		return facts;
	}

	public int maxRecordId() {
		return maxRecordId;
	}
}
