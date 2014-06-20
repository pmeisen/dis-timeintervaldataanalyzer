package net.meisen.dissertation.model.indexes.datarecord;

import java.io.InputStream;
import java.util.Collection;

import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.indexes.IIndexedCollection;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.Group;
import net.meisen.dissertation.model.persistence.Identifier;

/**
 * A {@code MetaIndex} is used to index meta information associated to data. It
 * is assumed that querying the meta-data is often done by selecting all or
 * specific values from the meta-data.
 * 
 * @author pmeisen
 * 
 */
public class MetaIndex implements IDataRecordIndex {

	private final IIndexedCollection dimensionsIndex;

	private Group persistentGroup = null;

	/**
	 * Constructor to create the {@code MetaIndex} for the specified
	 * {@code TidaModel}.
	 * 
	 * @param model
	 *            the {@code TidaModel} to create the {@code MetaIndex} for
	 */
	public MetaIndex(final TidaModel model) {
		this.dimensionsIndex = createIndex(model);
	}

	/**
	 * Creates the {@code MetaIndexDimensions} based on the specified
	 * {@code structure}.
	 * 
	 * @param model
	 *            the {@code TidaModel} to create the index for
	 * 
	 * @return the created {@code MetaIndexDimensions} indexed by a
	 *         {@code BaseIndexedCollection}
	 * 
	 * @see IIndexedCollection
	 * @see MetaIndexDimension
	 */
	protected IIndexedCollection createIndex(final TidaModel model) {
		final IndexKeyDefinition key = new IndexKeyDefinition(
				MetaIndexDimension.class, "getModelId");
		final IIndexedCollection index = model.getIndexFactory().create(key);
		final MetaDataModel metaModel = model.getMetaDataModel();
		if (metaModel == null) {
			return index;
		}
		final Collection<DescriptorModel<?>> descModels = metaModel
				.getDescriptorModels();
		if (descModels == null || descModels.size() == 0) {
			return index;
		}

		// create a MetaIndexDimension for each MetaStructureEntry
		for (final DescriptorModel<?> descModel : descModels) {

			// create an IndexDimension for the MetaInformation
			@SuppressWarnings({ "rawtypes", "unchecked" })
			final MetaIndexDimension idxDim = new MetaIndexDimension(descModel,
					model.getBitmapCache(), model.getIndexFactory());

			index.addObject(idxDim);
		}

		return index;
	}

	@Override
	public void index(final ProcessedDataRecord record) {

		for (final MetaIndexDimension<?> dim : getDimensions()) {
			dim.index(record);
		}
	}

	/**
	 * Gets the amount of dimensions indexed by {@code this}.
	 * 
	 * @return the amount of dimensions indexed
	 */
	public int getAmountOfDimensions() {
		return dimensionsIndex.size();
	}

	/**
	 * Gets the dimensions of the {@code MetaIndex} as collection.
	 * 
	 * @return the dimensions of the {@code MetaIndex}
	 */
	@SuppressWarnings("unchecked")
	protected Collection<MetaIndexDimension<?>> getDimensions() {
		return (Collection<MetaIndexDimension<?>>) dimensionsIndex.getAll();
	}

	@Override
	public void optimize() {
		for (final MetaIndexDimension<?> dim : getDimensions()) {
			dim.optimize();
		}
	}

	/**
	 * Get the {@code IndexDimension} for the specified {@code modelId}.
	 * 
	 * @param modelId
	 *            the
	 * 
	 * @return the {@code MetaIndexDimension} for the specified {@code modelId}.
	 */
	public MetaIndexDimension<?> get(final String modelId) {
		return (MetaIndexDimension<?>) dimensionsIndex.getObject(modelId);
	}

	@Override
	public void save(final BasePersistor persistor) {
		// nothing to save, the dimension are added via registration
	}

	@Override
	public void load(final BasePersistor persistor,
			final Identifier identifier, final InputStream inputStream) {
		throw new IllegalStateException("The '" + getClass().getSimpleName()
				+ "' does not save anything which should be loaded.");
	}

	@Override
	public void isRegistered(final BasePersistor persistor, final Group group) {
		this.persistentGroup = group;

		for (final MetaIndexDimension<?> dim : getDimensions()) {
			persistor.register(group.append("" + dim.getModelId()), dim);
		}
	}

	@Override
	public Group getPersistentGroup() {
		return persistentGroup;
	}
}
