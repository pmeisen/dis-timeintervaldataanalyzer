package net.meisen.dissertation.model.indexes.datarecord.slices;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.general.genmisc.types.Objects;

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
public class FactDescriptorSet {
	private final static Comparator<Descriptor<?, ?, ?>> valueComperator = new Comparator<Descriptor<?, ?, ?>>() {

		/**
		 * Compares the two {@code Descriptor} instances. If one of the
		 * descriptors isn't record invariant an exception is thrown.
		 * 
		 * @param desc1
		 *            the first {@code Descriptor} to compare
		 * @param desc2
		 *            the second {@code Descriptor} to compare
		 * 
		 * @return {@code 0} if both {@code Descriptor} instances are equal
		 *         considering their fact-values, {@code -1} if the fact-value
		 *         of the first {@code Descriptor} is smaller than the
		 *         fact-value of the second, and {@code 1} if the fact-value of
		 *         the first {@code Descriptor} is larger than the one of the
		 *         second value
		 * 
		 * @throws IllegalStateException
		 *             if one of the {@code Descriptor} instances isn't record
		 *             invariant (see {@link Descriptor#isRecordInvariant()}
		 * 
		 * @see Descriptor
		 */
		@Override
		public int compare(final Descriptor<?, ?, ?> desc1,
				final Descriptor<?, ?, ?> desc2) {

			// check nulls
			if (desc1 == null && desc2 == null) {
				return 0;
			} else if (desc1 == null) {
				return -1;
			} else if (desc2 == null) {
				return 1;
			}

			// make sure both are invariant
			final boolean invariantDesc1 = desc1.isRecordInvariant();
			final boolean invariantDesc2 = desc2.isRecordInvariant();
			if (invariantDesc1 && invariantDesc2) {
				final double factDesc1 = desc1.getFactValue(null);
				final double factDesc2 = desc2.getFactValue(null);

				if (factDesc1 < factDesc2) {
					return -1;
				} else if (factDesc1 > factDesc2) {
					return 1;
				}

				/*
				 * the models are equal within one set, which is ensured by the
				 * implementation, therefore just check the identifiers
				 */
				return Objects.compare(desc1.getId(), desc2.getId());
			} else {
				throw new IllegalStateException(
						"One of the descriptors is variant considering the used record, and therefore cannot be sorted.");
			}
		}
	};

	private Map<String, SortedSet<Descriptor<?, ?, ?>>> facts;

	/**
	 * Creates an empty set.
	 */
	public FactDescriptorSet() {
		facts = new HashMap<String, SortedSet<Descriptor<?, ?, ?>>>();
	}

	/**
	 * Creates a {@code FactDescriptorSet} with the specified
	 * {@code descriptors}.
	 * 
	 * @param descriptors
	 *            the {@code descriptors} to be added
	 */
	public FactDescriptorSet(final Collection<Descriptor<?, ?, ?>> descriptors) {
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
		SortedSet<Descriptor<?, ?, ?>> set = facts.get(modelId);
		if (set == null) {
			set = new TreeSet<Descriptor<?, ?, ?>>(getValueComparator());
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
	 * Gets a {@code Comparator} used to compare the values of instances of
	 * {@code Descriptor} instances. <br/>
	 * <br/>
	 * <b>Note:</b><br/>
	 * The {@code Descriptor} instances have to be record invariant, i.e.
	 * {@link Descriptor#isRecordInvariant()} must return {@code true}.
	 * 
	 * @return the {@code Comparator} to be used
	 * 
	 * @see Comparator
	 */
	protected Comparator<Descriptor<?, ?, ?>> getValueComparator() {
		return valueComperator;
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
		final SortedSet<Descriptor<?, ?, ?>> descriptors = facts
				.get(descriptorModelId);

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
	 * Gets the {@code SortedSet} of the descriptors added for the specified
	 * {@code DescriptorModel}.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} to get the set for
	 * @return the {@code SortedSet} for the specified {@code model}
	 * 
	 * @see DescriptorModel
	 * @see SortedSet
	 */
	public SortedSet<Descriptor<?, ?, ?>> getDescriptors(
			final DescriptorModel<?> model) {
		return facts.get(model.getId());
	}

	/**
	 * Gets the {@code SortedSet} of the descriptors added for the specified
	 * {@code descriptorModelId}.
	 * 
	 * @param descriptorModelId
	 *            the identifier of the {@code DescriptorModel} to get the set
	 *            for
	 * @return the {@code SortedSet} for the specified {@code model}
	 * 
	 * @see DescriptorModel
	 * @see SortedSet
	 */
	public SortedSet<Descriptor<?, ?, ?>> getDescriptors(
			final String descriptorModelId) {
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
		final SortedSet<Descriptor<?, ?, ?>> descriptors = facts
				.get(descriptorModelId);

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
		final SortedSet<Descriptor<?, ?, ?>> set = facts.get(descriptorModelId);
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
