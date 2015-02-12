package net.meisen.dissertation.model.indexes.datarecord.slices;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import net.meisen.dissertation.model.cache.IBitmapIdCacheable;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.descriptors.FactDescriptor;
import net.meisen.general.genmisc.resources.IByteBufferReader;
import net.meisen.general.genmisc.types.Streams;

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
public class FactDescriptorModelSet implements IBitmapIdCacheable,
		Iterable<FactDescriptor<?>> {
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
		return add(desc.getFactDescriptor());
	}

	/**
	 * Adds {@code factDesc} to {@code this}. The method returns {@code true} if
	 * the element wasn't in the set before, otherwise {@code false} is
	 * returned.
	 * 
	 * @param factDesc
	 *            the {@code FactDescriptor} to be added
	 * 
	 * @return {@code true} if the element wasn't in the set before, otherwise
	 *         {@code false}
	 */
	public boolean add(final FactDescriptor<?> factDesc) {
		if (factDesc == null) {
			throw new NullPointerException("Cannot add a null FactDescriptor.");
		}

		final String modelId = factDesc.getModelId();
		final FactDescriptorSet set = getSet(modelId);
		return addToSet(set, factDesc);
	}

	/**
	 * Gets the {@code FactDescriptorSet} for the specified {@code modelId}. The
	 * set is created if non for the specified model exists.
	 * 
	 * @param modelId
	 *            the identifier of the model to be created
	 * 
	 * @return the created or found {@code FactDescriptorSet}
	 */
	protected FactDescriptorSet getSet(final String modelId) {
		FactDescriptorSet set = facts.get(modelId);
		if (set == null) {
			set = new FactDescriptorSet();
			facts.put(modelId, set);
		}

		return set;
	}

	/**
	 * Adds the specified {@code FactDescriptor} to the specified
	 * {@code FactDescriptorSet}. The method does not validate if the
	 * {@code factDesc} fits to the {@code set}.
	 * 
	 * @param set
	 *            the {@code FactDescriptorSet} to add the
	 *            {@code FactDescriptor} to
	 * @param factDesc
	 *            the {@code FactDescriptor} to be added
	 * 
	 * @return {@code true} if the entry was added, otherwise {@code false}
	 */
	protected boolean addToSet(final FactDescriptorSet set,
			final FactDescriptor<?> factDesc) {
		return set.add(factDesc);
	}

	/**
	 * Combines {@code this} and the specified {@code factSet}.
	 * 
	 * @param factSet
	 *            the {@code FactDescriptorModelSet} to be combined with
	 *            {@code this}
	 */
	public void combine(final FactDescriptorModelSet factSet) {
		if (factSet == null) {
			return;
		}

		for (final Entry<String, FactDescriptorSet> e : factSet.facts
				.entrySet()) {
			final String modelId = e.getKey();
			final FactDescriptorSet set = getSet(modelId);

			// get the factDescriptor instances of the model
			for (final FactDescriptor<?> factDesc : e.getValue()) {
				addToSet(set, factDesc);
			}
		}
	}

	/**
	 * Tries to add all the {@code descriptors} to {@code this}.
	 * 
	 * @param descriptors
	 *            the {@code Descriptors} to be added
	 * 
	 * @return {@code true} if the set was modified because of the adding,
	 *         otherwise {@code false}
	 */
	public boolean addDescriptors(
			final Collection<Descriptor<?, ?, ?>> descriptors) {
		if (descriptors == null) {
			return false;
		}

		boolean modified = false;
		for (final Descriptor<?, ?, ?> desc : descriptors) {
			modified = addDescriptor(desc) || modified;
		}

		return modified;
	}

	/**
	 * Tries to add all the {@code descriptors} to {@code this}.
	 * 
	 * @param descriptors
	 *            the {@code Descriptors} to be added
	 * 
	 * @return {@code true} if the set was modified because of the adding,
	 *         otherwise {@code false}
	 */
	public boolean addDescriptors(final Descriptor<?, ?, ?>... descriptors) {
		if (descriptors == null) {
			return false;
		}

		boolean modified = false;
		for (final Descriptor<?, ?, ?> desc : descriptors) {
			modified = addDescriptor(desc) || modified;
		}

		return modified;
	}

	/**
	 * Tries to add all the {@code factDescriptors} to {@code this}.
	 * 
	 * @param factDescriptors
	 *            the {@code FactDescriptors} to be added
	 * 
	 * @return {@code true} if the set was modified because of the adding,
	 *         otherwise {@code false}
	 */
	public boolean add(final Collection<FactDescriptor<?>> factDescriptors) {
		if (factDescriptors == null) {
			return false;
		}

		boolean modified = false;
		for (final FactDescriptor<?> factDesc : factDescriptors) {
			modified = add(factDesc) || modified;
		}

		return modified;
	}

	/**
	 * Tries to add all the {@code factDescriptors} to {@code this}.
	 * 
	 * @param factDescriptors
	 *            the {@code FactDescriptors} to be added
	 * 
	 * @return {@code true} if the set was modified because of the adding,
	 *         otherwise {@code false}
	 */
	public boolean add(final FactDescriptor<?>... factDescriptors) {
		if (factDescriptors == null) {
			return false;
		}

		boolean modified = false;
		for (final FactDescriptor<?> factDesc : factDescriptors) {
			modified = add(factDesc) || modified;
		}

		return modified;
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
	 * Iterate over the sorted {@code FactDescriptor} instances of the set.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} to get the descriptors of
	 * 
	 * @return an {@code Iterable} instance to iterate over the different
	 *         {@code FactDescriptor} instances
	 * 
	 * @see Iterable
	 */
	public Iterable<FactDescriptor<?>> facts(final DescriptorModel<?> model) {
		return facts(model.getId());
	}

	/**
	 * Iterate over the sorted {@code FactDescriptor} instances of the set.
	 * 
	 * @param descriptorModelId
	 *            the identifier of the {@code DescriptorModel} to get the
	 *            descriptors of
	 * 
	 * @return an {@code Iterable} instance to iterate over the different
	 *         {@code FactDescriptor} instances
	 * 
	 * @see Iterable
	 */
	public Iterable<FactDescriptor<?>> facts(final String descriptorModelId) {
		final FactDescriptorSet descriptors = getDescriptors(descriptorModelId);

		return new Iterable<FactDescriptor<?>>() {

			@Override
			public Iterator<FactDescriptor<?>> iterator() {
				if (descriptors == null) {
					return Collections.<FactDescriptor<?>> emptyList()
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
	 * Creates a sorted list of the {@code FactDescriptor} instances of the
	 * specified {@code DescriptorModel}.
	 * 
	 * @param model
	 *            the {@code DescriptorModel}
	 * 
	 * @return the created list
	 * 
	 * @see DescriptorModel
	 */
	public List<FactDescriptor<?>> createSortedDescriptorList(
			final DescriptorModel<?> model) {
		return createSortedDescriptorList(model.getId());
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
			final String descriptorModelId) {
		final FactDescriptorSet descriptors = getDescriptors(descriptorModelId);

		final List<FactDescriptor<?>> sortedList = new ArrayList<FactDescriptor<?>>();
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
		addDescriptors(descriptors);
	}

	/**
	 * Sets the descriptors for the {@code Set}. All prior added descriptors are
	 * removed.
	 * 
	 * @param descriptors
	 *            the descriptors to be registered
	 */
	public void set(final Collection<FactDescriptor<?>> descriptors) {
		facts.clear();
		add(descriptors);
	}

	@Override
	public String toString() {
		return facts.toString();
	}

	@Override
	public Iterator<FactDescriptor<?>> iterator() {
		return new Iterator<FactDescriptor<?>>() {
			private Iterator<Entry<String, FactDescriptorSet>> it = facts
					.entrySet().iterator();
			private Iterator<FactDescriptor<?>> innerIt = null;

			@Override
			public boolean hasNext() {
				return it.hasNext() || (innerIt != null && innerIt.hasNext());
			}

			@Override
			public FactDescriptor<?> next() {

				// get the next inner iterator
				if (innerIt == null || !innerIt.hasNext()) {
					innerIt = it.next().getValue().iterator();
				}

				return innerIt.next();
			}

			@Override
			public void remove() {
				throw new UnsupportedOperationException(
						"The iterator is read only.");
			}
		};
	}

	/**
	 * Returns an iterator to iterate over the {@code FactDescriptor} instances
	 * of the specified {@code modelId}.
	 * 
	 * @param modelId
	 *            the identifier of the {@code DescriptorModel} to get the
	 *            {@code FactDescriptor} instances for
	 * 
	 * @return an iterator, never {@code null}
	 */
	public Iterable<FactDescriptor<?>> iterator(final String modelId) {
		final FactDescriptorSet set = facts.get(modelId);
		if (set == null) {
			return Collections.<FactDescriptor<?>> emptyList();
		} else {
			return set;
		}
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == this) {
			return true;
		} else if (obj instanceof FactDescriptorModelSet) {
			return facts.equals(((FactDescriptorModelSet) obj).facts);
		} else {
			return false;
		}
	}

	/**
	 * Serializes {@code this} to the specified {@code out}.
	 * 
	 * @param out
	 *            the stream to write to
	 * 
	 * @throws IOException
	 *             if {@code this} cannot be serialized
	 */
	public void serialize(final DataOutput out) throws IOException {
		final Set<String> models = getModels();
		out.write(Streams.intToByte(models.size()));

		// write each model
		for (final String modelId : models) {
			final FactDescriptorSet facts = getDescriptors(modelId);

			// write the modelIdentifier
			out.write(Streams.objectToByte(modelId));
			out.write(Streams.intToByte(facts.size()));

			// add the different values
			for (final FactDescriptor<?> fact : facts) {
				out.write(Streams.doubleToByte(fact.getFact()));
				out.write(Streams.booleanToByte(fact.isRecordInvariant()));
				out.write(Streams.objectToByte(fact.getId()));
			}
		}
	}

	/**
	 * Deserializes {@code this} instance from the specified {@code DataInput}.
	 * If {@code this} is currently filled, i.e. contains any data, the data is
	 * removed prior to loading.
	 * 
	 * @param in
	 *            the {@code DataInput} to read from
	 * 
	 * @return {@code this}
	 * 
	 * @throws IOException
	 *             if the data cannot be used for deserialization
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public FactDescriptorModelSet deserialize(final DataInput in)
			throws IOException {
		facts.clear();

		final IByteBufferReader reader = Streams.createByteBufferReader(in,
				1024);

		// get the amount of models
		final int modelsSize = Streams.readNextObject(reader, Integer.class);

		// read each model
		for (int i = 0; i < modelsSize; i++) {
			final String descModelId = (String) Streams.readNextObject(reader);
			final int factsSize = Streams.readNextObject(reader, Integer.class);

			// read the facts of the model
			for (int k = 0; k < factsSize; k++) {
				final double fact = Streams
						.readNextObject(reader, Double.class);
				final boolean invariant = Streams.readNextObject(reader,
						Boolean.class);
				final Object descId = Streams.readNextObject(reader);

				// create the FactDescriptor
				final FactDescriptor des;
				if (invariant) {
					des = new FactDescriptor(descModelId, descId, fact);
				} else {
					des = new FactDescriptor(descModelId, descId);
				}

				// add the descriptor
				add(des);
			}
		}

		return this;
	}
}
