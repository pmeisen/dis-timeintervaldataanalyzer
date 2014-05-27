package net.meisen.dissertation.model.indexes.datarecord.slices;

import java.io.DataInputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import net.meisen.dissertation.model.cache.IBitmapCache;
import net.meisen.dissertation.model.cache.IBitmapIdCacheable;
import net.meisen.dissertation.model.cache.IFactDescriptorModelSetCache;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.descriptors.FactDescriptor;

/**
 * A slice of a dimension which has associated {@code Descriptors}.
 * 
 * @author pmeisen
 * 
 * @param <I>
 *            the type of the identifier of the slice
 */
public class SliceWithDescriptors<I> extends BaseSlice<I> {
	private IFactDescriptorModelSetCache factsCache;

	private FactDescriptorModelSet facts = null;

	/**
	 * Default constructor to create a slice with descriptors.
	 * 
	 * @param sliceId
	 *            the identifier of the slice
	 */
	public SliceWithDescriptors(final SliceId<I> sliceId,
			final IBitmapCache bitmapCache,
			final IFactDescriptorModelSetCache factsCache) {
		super(sliceId, bitmapCache);

		this.factsCache = factsCache;
	}

	/**
	 * Marks the specified {@code recId} to be set, i.e. the value {@code this}
	 * slice represents by id is assumed to be set for the specified record.
	 * 
	 * @param recId
	 *            the identifiers of the record to be set
	 * 
	 * @param descriptors
	 *            the {@code Descriptor} instances associated to the
	 *            {@code DataRecord}
	 */
	public void set(final int recId, final Descriptor<?, ?, ?>... descriptors) {
		set(recId, descriptors == null ? null : Arrays.asList(descriptors));
	}

	/**
	 * Marks the specified {@code recId} to be set, i.e. the value {@code this}
	 * slice represents by id is assumed to be set for the specified record.
	 * 
	 * @param recId
	 *            the identifiers of the record to be set
	 * 
	 * @param descriptors
	 *            the {@code Descriptor} instances associated to the
	 *            {@code DataRecord}
	 */
	public void set(final int recId,
			final Collection<Descriptor<?, ?, ?>> descriptors) {
		if (descriptors == null) {
			return;
		}

		// modify the meta-data
		if (getFactsSet().addDescriptors(descriptors)) {
			updateFactsCache();
		}

		// modify the bitmap
		getBitmap().set(recId);
		updateBitmapCache();
	}

	public void deserialize(final DataInputStream in,
			final List<FactDescriptor<?>> factDescriptors) throws IOException {

		// set the descriptors of the slice
		getFactsSet().set(factDescriptors);
		updateFactsCache();

		// update the bitmap
		super.deserializeBitmap(in);
	}

	protected void updateFactsCache() {
		factsCache.cacheFactDescriptorModelSet(getSliceId(), getFactsSet());
	}

	/**
	 * An iterator to iterate over the identifiers of the registered
	 * {@code DescriptorModel} instances.
	 * 
	 * @return the {@code Iterable} for the identifiers
	 * 
	 * @see Iterable
	 */
	public Iterable<String> models() {
		return getFactsSet().models();
	}

	/**
	 * Iterate over the sorted {@code Descriptor} instances of the slice.
	 * 
	 * @param descriptorModelId
	 *            the identifier of the {@code DescriptorModel} to get the
	 *            descriptors for
	 * 
	 * @return an {@code Iterable} instance to iterate over the different
	 *         descriptor instances
	 * 
	 * @see Iterable
	 */
	public Iterable<FactDescriptor<?>> facts(final String descriptorModelId) {
		return getFactsSet().facts(descriptorModelId);
	}

	/**
	 * Gets the sorted set (sorted by value) of the descriptors.
	 * 
	 * @param descriptorModelId
	 *            the identifier of the {@code DescriptorModel} to get the
	 *            descriptors for
	 * 
	 * @return a sorted set of descriptors
	 */
	public FactDescriptorSet getDescriptors(final String descriptorModelId) {
		return getFactsSet().getDescriptors(descriptorModelId);
	}

	/**
	 * Gets the number of {@code DescriptorModel} instances associated to the
	 * slice.
	 * 
	 * @return the number of {@code DescriptorModel} instances associated to the
	 *         slice
	 */
	public int numberOfModels() {
		return getFactsSet().numberOfModels();
	}

	/**
	 * Gets the number of facts for a specific {@code DescriptorModel}.
	 * 
	 * @param descriptorModelId
	 *            the identifier of the model to get the number of facts for
	 * 
	 * @return the number of facts associated to the specified
	 *         {@code descriptorModelId}.
	 */
	public int numberOfFacts(final String descriptorModelId) {
		return getFactsSet().numberOfFacts(descriptorModelId);
	}

	/**
	 * Sets the descriptors for the {@code Slice}. All other descriptors are
	 * removed.
	 * 
	 * @param descriptors
	 *            the descriptors to be used for the {@code Slice}
	 */
	public void setDescriptors(final Collection<Descriptor<?, ?, ?>> descriptors) {
		getFactsSet().setDescriptors(descriptors);
	}

	/**
	 * Creates a list of the identifiers of the associated
	 * {@code DescriptorModel} instances.
	 * 
	 * @return the list of identifiers of the associated {@code DescriptorModel}
	 *         instances
	 */
	public List<String> createModelList() {
		return getFactsSet().createModelList();
	}

	/**
	 * Creates a sorted list of the {@code FactDescriptor} instances of the
	 * specified {@code DescriptorModel}.
	 * 
	 * @param descriptorModelId
	 *            the identifier of the {@code DescriptorModel}
	 * 
	 * @return the created list
	 * 
	 * @see DescriptorModel
	 */
	public List<FactDescriptor<?>> createSortedDescriptorList(
			String descriptorModelId) {
		return getFactsSet().createSortedDescriptorList(descriptorModelId);
	}

	@Override
	public String toString() {
		return super.toString() + " (" + getFactsSet() + ")";
	}

	@Override
	public void release(final IBitmapIdCacheable instance) {
		if (instance != null
				&& instance.getClass().equals(FactDescriptorModelSet.class)) {
			facts = null;
		} else {
			super.release(instance);
		}
	}

	/**
	 * Gets all the {@code DescriptorModels} associated to the slice. For each
	 * {@code DescriptorModel} the different associated {@code Descriptors} are
	 * available.
	 * 
	 * @return the {@code DescriptorModels} associated to the slice
	 */
	public FactDescriptorModelSet getFactsSet() {
		if (facts == null) {
			facts = factsCache.getSet(getSliceId());
		}

		return facts;
	}
}
