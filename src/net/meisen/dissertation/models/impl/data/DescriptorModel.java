package net.meisen.dissertation.models.impl.data;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.data.IIdsFactory;
import net.meisen.dissertation.data.impl.indexes.IndexedCollectionFactory;
import net.meisen.dissertation.exceptions.DescriptorModelException;
import net.meisen.dissertation.models.IMultipleKeySupport;
import net.meisen.dissertation.models.impl.indexes.IndexKeyDefinition;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * A {@code DescriptorModel} which generally defines {@code Descriptor}
 * instances.
 * 
 * @author pmeisen
 * 
 * @param <I>
 *            the type of the index used by this {@code DescriptorModel}
 * 
 */
@SuppressWarnings("rawtypes")
public class DescriptorModel<I extends Object> {
	private final String id;
	private final String name;
	private final Class<? extends Descriptor> descriptorClass;
	private final IIdsFactory<I> idsFactory;

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	@Autowired
	@Qualifier(DefaultValues.INDEXFACTORY_ID)
	private IndexedCollectionFactory indexedCollectionFactory;

	private IMultipleKeySupport descriptors;

	/**
	 * Constructor to define the {@code id} and the {@code dataType} of the
	 * model's data.
	 * 
	 * @param id
	 *            the id of the model
	 * @param descriptorClass
	 *            the class of the {@code Descriptors} hold by the
	 *            {@code DescriptorModel}.
	 * @param idsFactory
	 *            the {@code IIdsFactory} used to create indexes for the
	 *            {@code Descriptor}
	 */
	public DescriptorModel(final String id,
			final Class<? extends Descriptor> descriptorClass,
			final IIdsFactory<I> idsFactory) {
		this(id, id, descriptorClass, idsFactory);
	}

	/**
	 * Constructor to define the {@code id}, {@code name} and the
	 * {@code dataType} of the model's data.
	 * 
	 * @param id
	 *            the id of the model
	 * @param name
	 *            the name of the model
	 * @param descriptorClass
	 *            the class of the {@code Descriptors} hold by the
	 *            {@code DescriptorModel}.
	 * @param idsFactory
	 *            the {@code IIdsFactory} used to create indexes for the
	 *            {@code Descriptor}
	 */
	public DescriptorModel(final String id, final String name,
			final Class<? extends Descriptor> descriptorClass,
			final IIdsFactory<I> idsFactory) {
		this(id, name, descriptorClass, idsFactory, null);
	}

	/**
	 * Constructor to define the {@code id}, {@code name} and the
	 * {@code dataType} of the model's data.
	 * 
	 * @param id
	 *            the id of the model
	 * @param name
	 *            the name of the model
	 * @param descriptorClass
	 *            the class of the {@code Descriptors} hold by the
	 *            {@code DescriptorModel}.
	 * @param idsFactory
	 *            the {@code IIdsFactory} used to create indexes for the
	 *            {@code Descriptor}
	 * @param indexedCollectionFactory
	 *            the {@code IndexedCollectionFactory} which determines what
	 *            index to be used for the {@code Descriptor}
	 */
	public DescriptorModel(final String id, final String name,
			final Class<? extends Descriptor> descriptorClass,
			final IIdsFactory<I> idsFactory,
			final IndexedCollectionFactory indexedCollectionFactory) {
		this.id = id;
		this.name = name;
		this.descriptorClass = descriptorClass;
		this.idsFactory = idsFactory;
		this.indexedCollectionFactory = indexedCollectionFactory;
	}

	/**
	 * Gets the type of the {@code Descriptor}.
	 * 
	 * @return the type of the {@code Descriptor}
	 */
	public Class<? extends Descriptor> getDescriptorClass() {
		return descriptorClass;
	}

	/**
	 * Gets the name of the model.
	 * 
	 * @return the name of the model
	 */
	public String getName() {
		return name;
	}

	/**
	 * Gets the id of the model.
	 * 
	 * @return the id of the model
	 */
	public String getId() {
		return id;
	}

	@Override
	public boolean equals(final Object o) {
		boolean res = false;

		if (o == this) {
			res = true;
		} else if (o instanceof DescriptorModel) {
			final DescriptorModel cmp = (DescriptorModel) o;

			// check the attributes
			if (id.equals(cmp.getId())) {
				return true;
			}
		}

		return res;
	}

	/**
	 * Creates a {@code Descriptor} for each passed {@code value}.
	 * 
	 * @param values
	 *            the values to create {@code Descriptors} for
	 * @return the {@code Collection} of created {@code Descriptors}
	 * 
	 * @see Descriptor
	 */
	public Collection<Descriptor> createDescriptors(
			final Collection<Object> values) {
		final List<Descriptor> descriptors = new ArrayList<Descriptor>();

		for (final Object value : values) {
			descriptors.add(createDescriptor(value));
		}

		return descriptors;
	}

	/**
	 * Creates a {@code Descriptor} with the specified {@code value} using the
	 * definitions of {@code this} model.
	 * 
	 * @param value
	 *            the value to be added
	 * 
	 * @return the created {@code Descriptor}
	 * 
	 * @see Descriptor
	 */
	public <D, T extends Descriptor<D, T, I>> Descriptor<D, T, I> createDescriptor(
			final D value) {
		final Class<?> valueType = value.getClass();

		// get the constructor
		final Constructor<? extends Descriptor> constructor = findConstructor(
				descriptorClass, valueType);

		// create the instance and assign an id and a value
		final Descriptor descriptor;
		try {
			descriptor = constructor.newInstance(this, idsFactory.getId(),
					value);
		} catch (final Exception e) {
			exceptionRegistry.throwException(DescriptorModelException.class,
					1000, e, descriptorClass.getName());
			return null;
		}
		addDescriptor(descriptor);

		@SuppressWarnings("unchecked")
		final Descriptor<D, T, I> typedDescriptor = (Descriptor<D, T, I>) descriptor;
		return typedDescriptor;
	}

	/**
	 * Gets all the {@code Descriptors} of {@code this} model.
	 * 
	 * @return all the {@code Descriptors} of {@code this} model
	 */
	@SuppressWarnings("unchecked")
	public Collection<Descriptor<?, ?, I>> getDescriptors() {
		return (Collection<Descriptor<?, ?, I>>) getDescriptorIndex().getAll();
	}

	/**
	 * Gets the {@code Descriptor} with the specified {@code id}, can be
	 * {@code null} if the id cannot be found.
	 * 
	 * @param id
	 *            the id of the {@code Descriptor} to be returned
	 * 
	 * @return the {@code Descriptor} with the specified {@code id} or
	 *         {@code null} if no {@code Descriptor} with such an {@code id}
	 *         exists
	 */
	@SuppressWarnings("unchecked")
	public Descriptor<?, ?, I> getDescriptor(final I id) {
		return (Descriptor<?, ?, I>) getDescriptorIndex().getObjectByDefNr(0,
				id);
	}

	/**
	 * Gets the {@code Descriptor} with the specified {@code value}, can be
	 * {@code null} if the value cannot be found.
	 * 
	 * @param value
	 *            the value of the {@code Descriptor} to be returned
	 * 
	 * @return the {@code Descriptor} with the specified {@code value} or
	 *         {@code null} if no {@code Descriptor} with such an {@code value}
	 *         exists
	 */
	@SuppressWarnings("unchecked")
	public Descriptor<?, ?, I> getDescriptorByValue(final Object value) {
		return (Descriptor<?, ?, I>) getDescriptorIndex().getObjectByDefNr(1,
				value);
	}

	/**
	 * Gets the class of the identifier used to identify {@code Descriptor}
	 * instances of {@code this} model.
	 * 
	 * @return the class of the identifier
	 */
	public Class<?> getIdClass() {
		return idsFactory.getIdClass();
	}

	/**
	 * Adds the {@code Descriptors} to the index of {@code this} model.
	 * 
	 * @param descriptors
	 *            the descriptors to be added
	 */
	protected void addDescriptors(final Collection<Descriptor> descriptors) {

		if (descriptors != null) {
			for (final Descriptor descriptor : descriptors) {
				addDescriptor(descriptor);
			}
		}
	}

	/**
	 * Adds the {@code Descriptor} to the index of {@code this} model.
	 * 
	 * @param descriptor
	 *            the descriptor to be added
	 */
	protected void addDescriptor(final Descriptor descriptor) {
		boolean added = false;

		try {
			added = getDescriptorIndex().addObject(descriptor);
		} catch (final Exception e) {
			exceptionRegistry.throwException(DescriptorModelException.class,
					1003, descriptor, getId());
		}

		// make sure it was added
		if (!added) {
			exceptionRegistry.throwException(DescriptorModelException.class,
					1002, descriptor, getId());
		}
	}

	/**
	 * Gets the {@code Index} used to index the different {@code Descriptor}
	 * instances. This method never returns {@code null}, but throws an
	 * exception if the {@code indexedCollectionFactory} isn't defined by wiring
	 * or construction.
	 * 
	 * @return the {@code Index} used to index the different {@code Descriptor}
	 *         instances
	 * 
	 * @throws RuntimeException
	 *             can throw any exception if the
	 *             {@code indexedCollectionFactory} is {@code null}
	 */
	protected IMultipleKeySupport getDescriptorIndex() {
		if (descriptors == null) {
			final IndexKeyDefinition desIdDef = new IndexKeyDefinition(
					Descriptor.class, "getId");
			desIdDef.overrideType(0, getIdClass());
			final IndexKeyDefinition uniqueDesDef = new IndexKeyDefinition(
					Descriptor.class, "getValue");

			// create the descriptors index
			descriptors = indexedCollectionFactory.create(desIdDef,
					uniqueDesDef);
		}

		return descriptors;
	}

	private Constructor<? extends Descriptor> findConstructor(
			final Class<? extends Descriptor> clazz, final Class<?> valueType) {

		Constructor<? extends Descriptor> constructor;

		try {
			constructor = clazz.getConstructor(DescriptorModel.class,
					Object.class, valueType);
			return constructor;
		} catch (final NoSuchMethodException e) {
			constructor = null;

			// check if a default constructor is supported
			try {
				constructor = clazz.getConstructor(DescriptorModel.class,
						Object.class, Object.class);
			} catch (final NoSuchMethodException innerE) {
				constructor = null;
			}

			// if not throw the exception
			if (constructor == null) {
				exceptionRegistry.throwException(
						DescriptorModelException.class, 1001, e,
						clazz.getName(), idsFactory.getIdClass().getName(),
						valueType.getName());
				return null;
			} else {
				return constructor;
			}
		}
	}

	/**
	 * Gets the amount of descriptors in this {@code DescriptorModel}. If all
	 * descriptors are needed anyways it makes more sense to get those using
	 * {@code #getDescriptors()} and count the retrieved {@code Collection}.
	 * 
	 * @return the amount of descriptors
	 */
	public int size() {
		return getDescriptorIndex().size();
	}
}
