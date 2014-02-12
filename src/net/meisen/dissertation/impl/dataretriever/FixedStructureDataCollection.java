package net.meisen.dissertation.impl.dataretriever;

import java.util.List;

import net.meisen.dissertation.model.dataretriever.DataCollection;

/**
 * A {@code DataCollection} used to retrieve data, which is defined via
 * configuration.
 * 
 * @author pmeisen
 * 
 */
public class FixedStructureDataCollection extends DataCollection<String> {

	private final List<FixedStructureDataRetrieverConfigEntry> entries;
	private final FixedStructureQueryConfig query;

	/**
	 * Constructor to create a {@code DataCollection} for the specified
	 * {@code config} and the specified {@code query}.
	 * 
	 * @param config
	 *            the general {@code FixedStructureDataRetrieverConfig} the
	 *            collection is based on
	 * @param query
	 *            the {@code FixedStructureQueryConfig} the collection is based
	 *            on
	 */
	public FixedStructureDataCollection(
			final FixedStructureDataRetrieverConfig config,
			final FixedStructureQueryConfig query) {
		this.entries = config.getEntries();
		this.query = query;

		setNames(config.getNames());
	}

	@Override
	public FixedStructureDataIterator iterator() {
		return new FixedStructureDataIterator(this);
	}

	/**
	 * Gets the amount of data contained in the collection.
	 * 
	 * @return the amount of data contained in the collection
	 */
	public int size() {
		return query == null ? 0 : query.getAmount();
	}

	/**
	 * Gets the entries defined for the collection.
	 * 
	 * @return the entries defined for the collection
	 */
	public List<FixedStructureDataRetrieverConfigEntry> getEntries() {
		return entries;
	}

	@Override
	public void release() {
		// nothing to do
	}
}
