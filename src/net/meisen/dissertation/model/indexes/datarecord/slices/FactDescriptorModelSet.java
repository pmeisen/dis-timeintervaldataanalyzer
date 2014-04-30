package net.meisen.dissertation.model.indexes.datarecord.slices;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;

/**
 * An index used to keep the sorted facts of descriptors. That means that the
 * set can contain {@code Descriptor} instances of different
 * {@code DescriptorModels}. The implementation groups the different
 * {@code Descriptors} based on their {@code DescriptorModel}. Additionally, the
 * descriptors of each model are sorted according to their fact-values (i.e.
 * {@link Descriptor#getValue()}).
 * 
 * @author pmeisen
 * 
 * @see Descriptor
 * @see DescriptorModel
 * 
 */
public class FactDescriptorModelSet {
	private Map<String, FactDescriptorSet> facts;

	/**
	 * Creates an empty set.
	 */
	public FactDescriptorModelSet() {
		facts = new HashMap<String, FactDescriptorSet>();
	}

	/**
	 * Creates a {@code FactDescriptorModelSet} with the specified
	 * {@code descriptors}.
	 * 
	 * @param descriptors
	 *            the {@code descriptors} to be added
	 */
	public FactDescriptorModelSet(
			final Collection<Descriptor<?, ?, ?>> descriptors) {
		this();

		this.setDescriptors(descriptors);
	}

	/**
	 * Adds {@code desc} to {@code this}. The method returns {@code true} if the
	 * element wasn't in the set before, otherwise {@code false} is returned.
	 * 
	 * @param desc
	 *            the {@code Descriptor} to be added
	 * 
	 * @return {@code true} if the element wasn't in the set before, otherwise
	 *         {@code false}
	 */
	public boolean addDescriptor(final Descriptor<?, ?, ?> desc) {
		if (desc == null) {
			throw new NullPointerException("Cannot add a null descriptor.");
		}

		final String modelId = desc.getModelId();

		// get the set if we don't have one create it
		FactDescriptorSet set = facts.get(modelId);
		if (set == null) {
			set = new FactDescriptorSet();
			facts.put(modelId, set);
		}

		return set.add(desc);
	}

	/**
	 * Tries to add all the {@code descriptors} to {@code this}.
	 * 
	 * @param descriptors
	 *            the {@code Descriptors} to be added
	 */
	public void addDescriptors(final Collection<Descriptor<?, ?, ?>> descriptors) {
		if (descriptors == null) {
			return;
		}

		for (final Descriptor<?, ?, ?> desc : descriptors) {
			addDescriptor(desc);
		}
	}

	/**
	 * Tries to add all the {@code descriptors} to {@code this}.
	 * 
	 * @param descriptors
	 *            the {@code Descriptors} to be added
	 */
	public void addDescriptors(final Descriptor<?, ?, ?>... descriptors) {
		if (descriptors == null) {
			return;
		}

		for (final Descriptor<?, ?, ?> desc : descriptors) {
			addDescriptor(desc);
		}
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
		final Set<String> keys = facts.keySet();

		return new Iterable<String>() {

			@Override
			public Iterator<String> iterator() {
				if (keys == null) {
					return Collections.<String> emptyList().iterator();
				} else {
					return keys.iterator();
				}
			}
		};
	}

	/**
	 * Gets the {@code Set} of the identifiers of the registered
	 * {@code DescriptorModel} instances.
	 * 
	 * @return the {@code Set} of the identifiers of the registered
	 *         {@code DescriptorModel} instances
	 * 
	 * @see DescriptorModel
	 * @see Set
	 */
	public Set<String> getModels() {
		return facts.keySet();
	}

	/**
	 * Creates a list of the identifiers of the registered
	 * {@code DescriptorModel} instances.
	 * 
	 * @return the list of identifiers of the registered {@code DescriptorModel}
	 *         instances
	 */
	public List<String> createModelList() {
		final List<String> list = new ArrayList<String>();
		list.addAll(facts.keySet());

		return list;
	}

	/**
	 * Iterate over the sorted {@code Descriptor} instances of the set.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} to get the descriptors of
	 * 
	 * @return an {@code Iterable} instance to iterate over the different
	 *         descriptor instances
	 * 
	 * @see Iterable
	 */
	public Iterable<Descriptor<?, ?, ?>> facts(final DescriptorModel<?> model) {
		return facts(model.getId());
	}

	/**
	 * Iterate over the sorted {@code Descriptor} instances of the set.
	 * 
	 * @param descriptorModelId
	 *            the identifier of the {@code DescriptorModel} to get the
	 *            descriptors of
	 * 
	 * @return an {@code Iterable} instance to iterate over the different
	 *         descriptor instances
	 * 
	 * @see Iterable
	 */
	public Iterable<Descriptor<?, ?, ?>> facts(final String descriptorModelId) {
		final FactDescriptorSet descriptors = getDescriptors(descriptorModelId);

		return new Iterable<Descriptor<?, ?, ?>>() {

			@Override
			public Iterator<Descriptor<?, ?, ?>> iterator() {
				if (descriptors == null) {
					return Collections.<Descriptor<?, ?, ?>> emptyList()
							.iterator();
				} else {
					return descriptors.iterator();
				}
			}
		};
	}

	/**
	 * Gets the {@code FactDescriptorSet} of the descriptors added for the
	 * specified {@code DescriptorModel}.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} to get the set for
	 * @return the {@code FactDescriptorSet} for the specified {@code model}
	 * 
	 * @see DescriptorModel
	 * @see FactDescriptorSet
	 */
	public FactDescriptorSet getDescriptors(final DescriptorModel<?> model) {
		return getDescriptors(model.getId());
	}

	/**
	 * Gets the {@code FactDescriptorSet} of the descriptors added for the
	 * specified {@code descriptorModelId}.
	 * 
	 * @param descriptorModelId
	 *            the identifier of the {@code DescriptorModel} to get the set
	 *            for
	 * @return the {@code FactDescriptorSet} for the specified {@code model}
	 * 
	 * @see DescriptorModel
	 * @see FactDescriptorSet
	 */
	public FactDescriptorSet getDescriptors(final String descriptorModelId) {
		return facts.get(descriptorModelId);
	}

	/**
	 * Creates a sorted list of the descriptors of the specified
	 * {@code DescriptorModel}.
	 * 
	 * @param model
	 *            the {@code DescriptorModel}
	 * 
	 * @return the created list
	 * 
	 * @see DescriptorModel
	 */
	public List<Descriptor<?, ?, ?>> createSortedDescriptorList(
			final DescriptorModel<?> model) {
		return createSortedDescriptorList(model.getId());
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
			final String descriptorModelId) {
		final FactDescriptorSet descriptors = getDescriptors(descriptorModelId);

		final List<Descriptor<?, ?, ?>> sortedList = new ArrayList<Descriptor<?, ?, ?>>();
		if (descriptors != null) {
			sortedList.addAll(descriptors);
		}

		return sortedList;
	}

	/**
	 * Gets the number of registered models.
	 * 
	 * @return the number of registered models
	 */
	public int numberOfModels() {
		return facts.size();
	}

	/**
	 * Gets the number of facts for a specific {@code DescriptorModel}.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} to get the number of facts for
	 * 
	 * @return the number of facts associated to the specified
	 *         {@code descriptorModelId}.
	 */
	public int numberOfFacts(final DescriptorModel<?> model) {
		return numberOfFacts(model.getId());
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
		final FactDescriptorSet set = getDescriptors(descriptorModelId);
		if (set == null) {
			return 0;
		}

		return set.size();
	}

	/**
	 * Sets the descriptors for the {@code Set}. All prior added descriptors are
	 * removed.
	 * 
	 * @param descriptors
	 *            the descriptors to be registered
	 */
	public void setDescriptors(final Collection<Descriptor<?, ?, ?>> descriptors) {
		facts.clear();
		for (final Descriptor<?, ?, ?> desc : descriptors) {
			addDescriptor(desc);
		}
	}

	@Override
	public String toString() {
		return facts.toString();
	}
}
