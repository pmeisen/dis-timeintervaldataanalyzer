package net.meisen.dissertation.model.indexes.tida;

import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.datastructure.MetaStructureEntry;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.indexes.BaseIndexedCollectionFactory;
import net.meisen.dissertation.model.indexes.IIndexedCollection;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;

public class MetaIndexDimension<I> {

	private final DescriptorModel<I> model;
	private final MetaStructureEntry metaEntry;

	private final IIndexedCollection index;

	public MetaIndexDimension(final MetaStructureEntry metaEntry,
			final DescriptorModel<I> model,
			final BaseIndexedCollectionFactory baseIndexedCollectionFactory) {
		this.metaEntry = metaEntry;
		this.model = model;

		// create an index to handle the different values for a descriptor
		final IndexKeyDefinition indexKeyDef = new IndexKeyDefinition(
				IndexBitmapSlice.class, "getId");
		indexKeyDef.overrideType(0, model.getIdClass());
		this.index = baseIndexedCollectionFactory.create(indexKeyDef);
	}

	public void add(final int recId, final IDataRecord rec) {
		if (rec == null) {
			return;
		}

		// get the id
		final I id = getIdFromRecord(rec);

		// create or get the slices
		IndexBitmapSlice<I> slice = getSliceById(id);
		if (slice == null) {
			slice = new IndexBitmapSlice<I>(id);
			index.addObject(slice);
		}
		slice.set(recId);
	}

	@SuppressWarnings("unchecked")
	public IndexBitmapSlice<I> getSliceById(final I id) {
		return (IndexBitmapSlice<I>) index.getObject(id);
	}

	public IndexBitmapSlice<I> getSliceByValue(final Object value) {
		final I id = getIdForValue(value);
		return getSliceById(id);
	}

	public int getAmountOfSlices() {
		return index.size();
	}

	public int[] getByValue(final Object value) {
		final I id = getIdForValue(value);
		return getById(id);
	}

	public int[] getById(final I id) {
		final IndexBitmapSlice<I> slice = getSliceById(id);
		if (slice == null) {
			return new int[0];
		} else {
			return slice.get();
		}
	}

	protected I getIdForValue(final Object value) {
		final Descriptor<?, ?, I> desc = model.getDescriptorByValue(value);

		if (desc == null) {
			// TODO we could add it couldn't we?!
			throw new NullPointerException("The value '" + value
					+ "' doesn't have any descriptor.");
		} else {
			return desc.getId();
		}
	}

	protected I getIdFromRecord(final IDataRecord record) {
		if (record == null) {
			throw new NullPointerException("The record cannot be null.");
		}

		final String name = metaEntry.getName();

		// determine the value which is of interest for the record
		final Object value;
		if (name == null) {
			value = record.getValue(metaEntry.getPosition());
		} else {
			value = record.getValue(name);
		}

		return getIdForValue(value);
	}
}
