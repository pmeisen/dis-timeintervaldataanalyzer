package net.meisen.dissertation.model.indexes.tida;

import java.util.ArrayList;
import java.util.List;

import net.meisen.dissertation.model.data.DataStructure;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.datastructure.MetaStructureEntry;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.indexes.BaseIndexedCollectionFactory;

public class TidaIndex {

	private final List<MetaIndexDimension<?>> dimensions;

	int id;

	public TidaIndex(final DataStructure structure,
			final MetaDataModel metaDataModel,
			final BaseIndexedCollectionFactory baseIndexedCollectionFactory) {

		final List<MetaIndexDimension<?>> idxDimensions = new ArrayList<MetaIndexDimension<?>>();
		final List<MetaStructureEntry> metaEntries = structure
				.getEntriesByClass(MetaStructureEntry.class);
		for (final MetaStructureEntry metaEntry : metaEntries) {
			final String descModelId = metaEntry.getDescriptorModel();
			final DescriptorModel<?> descModel = metaDataModel
					.getDescriptorModel(descModelId);

			// create an IndexDimension for the MetaInformation
			@SuppressWarnings({ "rawtypes", "unchecked" })
			final MetaIndexDimension idxDim = new MetaIndexDimension(metaEntry,
					descModel, baseIndexedCollectionFactory);
			idxDimensions.add(idxDim);
		}

		this.id = 0;
		this.dimensions = idxDimensions;
	}

	public TidaIndex(final List<MetaIndexDimension<?>> idxDimensions) {
		this.dimensions = idxDimensions;
	}

	public void index(final IDataRecord record) {
		for (final MetaIndexDimension<?> dim : dimensions) {
			dim.add(id, record);
			id++;
		}
	}
}
