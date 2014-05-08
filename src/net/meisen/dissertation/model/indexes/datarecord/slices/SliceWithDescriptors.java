package net.meisen.dissertation.model.indexes.datarecord.slices;

import java.io.DataInputStream;
import java.io.IOException;
import java.util.Collection;
import java.util.List;

import net.meisen.dissertation.model.cache.IBitmapCache;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;

/**
 * A slice of a dimension which has associated {@code Descriptors}.
 * 
 * @author pmeisen
 * 
 * @param <I>
 *            the type of the identifier of the slice
 */
public class SliceWithDescriptors<I> extends BaseSlice<I> {
	private final FactDescriptorModelSet facts;

	/**
	 * Default constructor to create a slice with descriptors.
	 * 
	 * @param sliceId
	 *            the identifier of the slice
	 */
	public SliceWithDescriptors(final SliceId<I> sliceId,
			final IBitmapCache cache) {
		super(sliceId, cache);

		facts = new FactDescriptorModelSet();
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
		getBitmap().set(recId);
		facts.addDescriptors(descriptors);
	}

	public void deserialize(final DataInputStream in,
			final List<Descriptor<?, ?, ?>> descriptors) throws IOException {

		// set the descriptors of the slice
		facts.addDescriptors(descriptors);

		// update the bitmap
		super.deserializeBitmap(in);
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
		getBitmap().set(recId);
		facts.addDescriptors(descriptors);

		// inform the cache
		updateBitmapCache();
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
		return facts.models();
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
	public Iterable<Descriptor<?, ?, ?>> facts(final String descriptorModelId) {
		return facts.facts(descriptorModelId);
	}

	/**
	 * Gets all the {@code DescriptorModels} associated to the slice. For each
	 * {@code DescriptorModel} the different associated {@code Descriptors} are
	 * available.
	 * 
	 * @return the {@code DescriptorModels} associated to the slice
	 */
	public FactDescriptorModelSet getDescriptorModels() {
		return facts;
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
		return facts.getDescriptors(descriptorModelId);
	}

	/**
	 * Gets the number of {@code DescriptorModel} instances associated to the
	 * slice.
	 * 
	 * @return the number of {@code DescriptorModel} instances associated to the
	 *         slice
	 */
	public int numberOfModels() {
		return facts.numberOfModels();
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
		return facts.numberOfFacts(descriptorModelId);
	}

	/**
	 * Sets the descriptors for the {@code Slice}. All other descriptors are
	 * removed.
	 * 
	 * @param descriptors
	 *            the descriptors to be used for the {@code Slice}
	 */
	public void setDescriptors(final Collection<Descriptor<?, ?, ?>> descriptors) {
		facts.setDescriptors(descriptors);
	}

	/**
	 * Creates a list of the identifiers of the associated
	 * {@code DescriptorModel} instances.
	 * 
	 * @return the list of identifiers of the associated {@code DescriptorModel}
	 *         instances
	 */
	public List<String> createModelList() {
		return facts.createModelList();
	}

	/**
	 * Creates a sorted list of the descriptors of the specified
	 * {@code DescriptorModel}.
	 * 
	 * @param descriptorModelId
	 *            the identifier of the {@code DescriptorModel}
	 * 
	 * @return the created list
	 * 
	 * @see DescriptorModel
	 */
	public List<Descriptor<?, ?, ?>> createSortedDescriptorList(
			String descriptorModelId) {
		return facts.createSortedDescriptorList(descriptorModelId);
	}

	@Override
	public String toString() {
		return super.toString() + " (" + facts + ")";
	}
}
