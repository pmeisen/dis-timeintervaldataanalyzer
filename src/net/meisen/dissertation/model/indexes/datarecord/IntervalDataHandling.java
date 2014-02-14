package net.meisen.dissertation.model.indexes.datarecord;

import java.util.ArrayList;
import java.util.List;

/**
 * Defines how the interval-data should be handled if it cannot be found within
 * the value of the {@code DataRecord} is {@code null}.
 * 
 * @author pmeisen
 * 
 */
public enum IntervalDataHandling {
	/**
	 * Use the boundaries (i.e. start or end) if the value of the start or end
	 * is {@code null}.
	 */
	BOUNDARIESWHENNULL("boundaries"),
	/**
	 * Fail if one of the values is {@code null}.
	 */
	FAILONNULL("fail"),
	/**
	 * Uses the start value for the end value if the end is {@code null} or uses
	 * the end value for the start value if the start is {@code null}. If both
	 * are {@code null} the {@link #BOUNDARIESWHENNULL} strategy is applied.
	 */
	USEOTHER("other", "others");

	private final List<String> synonyms;

	private IntervalDataHandling(final String... synonyms) {
		this.synonyms = new ArrayList<String>();
		this.synonyms.add(this.name().toLowerCase());

		if (synonyms != null) {
			for (final String synonym : synonyms) {
				this.synonyms.add(synonym.toLowerCase());
			}
		}
	}

	private boolean isSynonym(final String name) {
		return this.synonyms.contains(name.toLowerCase());
	}

	/**
	 * Finds the {@code IntervalDataHandling} associated to the specified
	 * {@code name}. If the name cannot be associated the default
	 * {@link #BOUNDARIESWHENNULL} will be returned.
	 * 
	 * @param name
	 *            the name of the {@code IntervalDataHandling} to retrieve
	 * 
	 * @return the {@code IntervalDataHandling} associated
	 */
	public static IntervalDataHandling find(final String name) {
		if (name != null) {
			for (final IntervalDataHandling intervalDataHandling : IntervalDataHandling
					.values()) {
				if (intervalDataHandling.isSynonym(name)) {
					return intervalDataHandling;
				}
			}
		}

		return BOUNDARIESWHENNULL;
	}
}
