package net.meisen.dissertation.model.data;

import java.util.ArrayList;
import java.util.List;

/**
 * Defines how the system should handle failing data request from different data
 * retrievers.
 * 
 * @author pmeisen
 * 
 */
public enum OfflineMode {
	/**
	 * The system assumes that it is offline and that this is to be expected
	 */
	TRUE("true", "yes", "y"),
	/**
	 * The system assumes that it is online and that data should be available,
	 * otherwise exceptions are thrown
	 */
	FALSE("false", "no", "n"),
	/**
	 * The system decides by situation if an offline situation is acceptable and
	 * decides by itself if an exception should be thrown or not.
	 */
	AUTO("auto");

	private final List<String> synonyms;

	private OfflineMode(final String... synonyms) {
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
	 * Finds the {@code OfflineMode} associated to the specified {@code name}.
	 * If the name cannot be associated the default {@link #FALSE} will be
	 * returned.
	 * 
	 * @param name
	 *            the name of the {@code OfflineMode} to retrieve
	 * 
	 * @return the {@code OfflineMode} associated
	 */
	public static OfflineMode find(final String name) {
		if (name != null) {
			for (final OfflineMode offlineMode : OfflineMode.values()) {
				if (offlineMode.isSynonym(name)) {
					return offlineMode;
				}
			}
		}

		return FALSE;
	}
}
