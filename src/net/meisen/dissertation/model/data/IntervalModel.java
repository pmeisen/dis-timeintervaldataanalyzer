package net.meisen.dissertation.model.data;

import java.util.List;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry.IntervalTypeFactory.IntervalType;
import net.meisen.dissertation.model.indexes.BaseIndexedCollectionFactory;
import net.meisen.dissertation.model.indexes.IIndexedCollection;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.datarecord.IntervalIndexDimension;
import net.meisen.dissertation.model.time.mapper.BaseMapperFactory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public class IntervalModel {

	@Autowired
	@Qualifier(DefaultValues.INDEXFACTORY_ID)
	private BaseIndexedCollectionFactory indexedCollectionFactory;

	@Autowired
	@Qualifier(DefaultValues.MAPPERFACTORY_ID)
	private BaseMapperFactory mapperFactory;

	/**
	 * Creates a {@code IntervalModel}. The instance must be wired prior to it's
	 * usage to ensure that a {@code baseIndexedCollectionFactory} is available.
	 */
	public IntervalModel() {
		this(null, null);
	}

	/**
	 * Creates a {@code IntervalModel} with the specified
	 * {@code baseIndexedCollectionFactory}, which should not be {@code null}.
	 * If the {@code baseIndexedCollectionFactory} should be {@code null} use
	 * another constructor and read its information.
	 * 
	 * @param indexedCollectionFactory
	 *            the {@code BaseIndexedCollectionFactory} used to determine the
	 *            indexes to be used
	 * 
	 * @see BaseIndexedCollectionFactory
	 */
	public IntervalModel(
			final BaseIndexedCollectionFactory indexedCollectionFactory,
			final BaseMapperFactory mapperFactory) {

		// set the factories
		this.indexedCollectionFactory = indexedCollectionFactory;
		this.mapperFactory = mapperFactory;
	}

	public IIndexedCollection createIndex(final DataStructure structure) {

		// TODO define the method needed for indexing - this are just the
		// dimensions so maybe a hierarchy?!
		final IndexKeyDefinition key = new IndexKeyDefinition(
				IntervalIndexDimension.class, "");
		final IIndexedCollection index = getIndexedCollectionFactory().create(
				key);
		if (structure == null) {
			return index;
		}

		// determine the start and end of the dataRecord
		final List<IntervalStructureEntry> entries = structure
				.getEntriesByClass(IntervalStructureEntry.class);

		// search for start and end
		IntervalStructureEntry start = null, end = null;
		for (final IntervalStructureEntry entry : entries) {
			final IntervalType type = entry.getType();

			// determine the start and end of the interval
			if (IntervalType.START.equals(type)) {
				if (start != null) {
					// TODO add exception
				}
				start = entry;
			} else if (IntervalType.END.equals(type)) {
				if (end != null) {
					// TODO add exception
				}
				end = entry;
			} else {
				// TODO add exception
			}
		}

		// add the different dimensions to the index
		createIndexDimension(start, end);

		return index;
	}

	public IntervalIndexDimension createIndexDimension(
			final IntervalStructureEntry start, final IntervalStructureEntry end) {
		final IntervalIndexDimension dim = new IntervalIndexDimension(start,
				end, getIndexedCollectionFactory());

		return dim;
	}

	/**
	 * Gets the {@code baseIndexedCollectionFactory} specified for the
	 * {@code IntervalModel}. This method should never return {@code null} if
	 * the {@code IntervalModel} is assumed to be initialized.
	 * 
	 * @return the {@code baseIndexedCollectionFactory} specified for the
	 *         {@code IntervalModel}
	 */
	public BaseIndexedCollectionFactory getIndexedCollectionFactory() {
		return indexedCollectionFactory;
	}

	public BaseMapperFactory getMapperFactory() {
		return mapperFactory;
	}
}
