package net.meisen.dissertation.impl.cache;

import net.meisen.dissertation.impl.data.metadata.LoadedMetaData;
import net.meisen.dissertation.model.cache.IMetaDataCache;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.metadata.MetaDataCollection;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;

/**
 * Utility methods for the {@code MetaDataCache}.
 * 
 * @author pmeisen
 * 
 * @see IMetaDataCache
 */
public class UtilMetaDataCache {

	/**
	 * Method used to create a {@code MetaDataCollection} for the specified
	 * {@code MetaDataModel}.
	 * 
	 * @param model
	 *            the {@code MetaDataModel} to create the
	 *            {@code MetaDataCollection} for
	 * 
	 * @return the created {@code MetaDataCollection}
	 */
	public static MetaDataCollection createCollectionForModel(
			final MetaDataModel model) {

		// create a new collection with the data of the model
		final MetaDataCollection metaDataCollection = new MetaDataCollection();
		for (final DescriptorModel<?> dm : model.getDescriptorModels()) {
			final String dmId = dm.getId();
			final LoadedMetaData metaData = new LoadedMetaData(dmId);

			// add the metaData
			for (final Descriptor<?, ?, ?> desc : dm.getAllDescriptors()) {
				final Object id = desc.getId();
				final Object value = desc.getValue();

				metaData.addValue(id, value);
			}

			// add the metaData
			metaDataCollection.addMetaData(metaData);
		}

		return metaDataCollection;
	}
}
