package net.meisen.dissertation.model.descriptors;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.DescriptorModelException;
import net.meisen.dissertation.model.dataretriever.BaseDataRetriever;
import net.meisen.dissertation.model.dataretriever.DataCollection;
import net.meisen.dissertation.model.dataretriever.IQueryConfiguration;
import net.meisen.dissertation.model.idfactories.IIdsFactory;
import net.meisen.dissertation.model.indexes.BaseIndexedCollectionFactory;
import net.meisen.dissertation.model.indexes.IMultipleKeySupport;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
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

	private NullDescriptor<I> nullDescriptor = null;
	private boolean failOnDuplicates = true;
	private boolean supportsNullDescriptor = false;

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	@Autowired
	@Qualifier(DefaultValues.INDEXFACTORY_ID)
	private BaseIndexedCollectionFactory baseIndexedCollectionFactory;

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
	 * @param baseIndexedCollectionFactory
	 *            the {@code BaseIndexedCollectionFactory} which determines what
	 *            index to be used for the {@code Descriptor}
	 */
	public DescriptorModel(final String id, final String name,
			final Class<? extends Descriptor> descriptorClass,
			final IIdsFactory<I> idsFactory,
			final BaseIndexedCollectionFactory baseIndexedCollectionFactory) {
		this.id = id;
		this.name = name;
		this.descriptorClass = descriptorClass;
		this.idsFactory = idsFactory;
		this.baseIndexedCollectionFactory = baseIndexedCollectionFactory;
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
	 * Create {@code Descriptor} instances from the passed {@code retriever}
	 * using the specified {@code query}.
	 * 
	 * @param retriever
	 *            the retrieve to be used to retrieve the data
	 * @param query
	 *            the query used to retrieve the data
	 * 
	 * @return the the {@code Collection} of created {@code Descriptors}
	 */
	public Collection<Descriptor> createDescriptors(
			final BaseDataRetriever retriever, final IQueryConfiguration query) {

		// get the loader to retrieve the data
		final DataCollection<?> loader = retriever.retrieve(query);

		// retrieve the data
		final Collection<Descriptor> data = createDescriptors(loader
				.transform());

		// release the loader
		loader.release();

		return data;
	}

	/**
	 * Creates a {@code Descriptor} with the specified {@code value} using the
	 * definitions of {@code this} model. <br/>
	 * <br/>
	 * 
	 * <b>Note</b> (considering {@code null}-values):<br/>
	 * If {@code null} will be passed and {@code null} values are supported
	 * (i.e. {@link #supportsNullDescriptor()} returns {@code true}) the method
	 * will return an instance of a {@code NullDescriptor}. A
	 * {@code NullDescriptor} does not have to be created and is only created
	 * once whenever needed. Therefore the method can be called several times
	 * without throwing an exception if {@code null} is passed as {@code value}
	 * and if it is supported.
	 * 
	 * @param value
	 *            the value to be added
	 * 
	 * @return the created {@code Descriptor} or the {@code NullDescriptor} of
	 *         this {@code DescriptorModel} if supported and {@code null} is
	 *         passed
	 * 
	 * @see Descriptor
	 * @see NullDescriptor
	 */
	public <D> Descriptor<D, ?, I> createDescriptor(final D value) {

		if (value == null) {
			@SuppressWarnings("unchecked")
			final Descriptor<D, ?, I> descriptor = (Descriptor<D, ?, I>) getNullDescriptor();
			return descriptor;
		} else {
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
				exceptionRegistry.throwException(
						DescriptorModelException.class, 1000, e,
						descriptorClass.getName());
				return null;
			}
			addDescriptor(descriptor);

			@SuppressWarnings("unchecked")
			final Descriptor<D, ?, I> typedDescriptor = (Descriptor<D, ?, I>) descriptor;
			return typedDescriptor;
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

	/**
	 * Gets the behavior of the {@code DescriptorModel} whenever a
	 * {@code Descriptor} is created using {@link #createDescriptor(Object)}
	 * which already exists. If no exception is thrown, i.e. the method returns
	 * {@code false}, the already created {@code Descriptor} is returned instead
	 * of creating another one.
	 * 
	 * @return {@code true} if the {@code DescriptorModel} throws an exception,
	 *         otherwise {@code false}
	 */
	public boolean isFailOnDuplicates() {
		return failOnDuplicates;
	}

	/**
	 * Defines if the {@code DescriptorModel} should throw an exception whenever
	 * a {@code Descriptor} is created using {@link #createDescriptor(Object)}.
	 * If no exception is thrown, i.e. set the value to {@code false}, the
	 * already created {@code Descriptor} is returned instead of creating
	 * another one.
	 * 
	 * @param failOnDuplicates
	 *            {@code true} if the {@code DescriptorModel} should throw an
	 *            exception, otherwise {@code false}
	 */
	public void setFailOnDuplicates(final boolean failOnDuplicates) {
		this.failOnDuplicates = failOnDuplicates;
	}

	/**
	 * Gets the behavior of the {@code DescriptorModel} considering {@code null}
	 * values. If {@code true} is returned, {@code null} values can be added,
	 * i.e. {@link #createDescriptor(Object)} can be called with {@code null}.
	 * 
	 * @return {@code true} if {@code null} values are supported, otherwise
	 *         {@code false}
	 */
	public boolean supportsNullDescriptor() {
		return supportsNullDescriptor;
	}

	/**
	 * Defines if the {@code DescriptorModel} supports {@code null} values. If
	 * set to {@code true} {@code null} values can be added, i.e.
	 * {@link #createDescriptor(Object)} can be called with {@code null}.
	 * 
	 * @param supportsNull
	 *            {@code true} if {@code null} values are supported, otherwise
	 *            {@code false}
	 */
	public void setSupportsNullDescriptor(final boolean supportsNull) {
		this.supportsNullDescriptor = supportsNull;
	}

	/**
	 * Gets the {@code Descriptor} which is used to identify {@code null}
	 * values. There is exactly one such {@code NullDescriptor} created for a
	 * {@code DescriptorModel} whenever the {@code DescriptorModel} supports
	 * {@code null} values.
	 * 
	 * @return the {@code NullDescriptor} of {@code this}, cannot be
	 *         {@code null}
	 * 
	 * @throws DescriptorModelException
	 *             if the {@code DescriptorModel} doesn't support {@code null}
	 *             values
	 * 
	 * @see NullDescriptor
	 * @see #supportsNullDescriptor()
	 */
	public NullDescriptor<I> getNullDescriptor()
			throws DescriptorModelException {

		if (!supportsNullDescriptor()) {
			exceptionRegistry.throwRuntimeException(
					DescriptorModelException.class, 1004, getId());
		}

		// make sure we have a nullDescriptor
		if (nullDescriptor == null) {
			nullDescriptor = new NullDescriptor<I>(this, idsFactory.getId());
		}

		return nullDescriptor;
	}

	/**
	 * Gets the identifier of the {@code Descriptor} which is used to identify
	 * {@code null} values. There is exactly one such {@code NullDescriptor}
	 * created for a {@code DescriptorModel} whenever the
	 * {@code DescriptorModel} supports {@code null} values.
	 * 
	 * @return the identifier of the {@code NullDescriptor} of {@code this},
	 *         cannot be {@code null}
	 * 
	 * @throws DescriptorModelException
	 *             if the {@code DescriptorModel} doesn't support {@code null}
	 *             values
	 * 
	 * @see NullDescriptor
	 * @see #supportsNullDescriptor()
	 */
	public I getNullDescriptorId() {
		return getNullDescriptor().getId();
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

		if (id == null) {
			throw new IllegalStateException("MICE ERROR HERE");
		} else if (supportsNullDescriptor() && id.equals(getNullDescriptorId())) {
			return nullDescriptor;
		} else {
			return (Descriptor<?, ?, I>) getDescriptorIndex().getObjectByDefNr(
					0, id);
		}
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

		if (supportsNullDescriptor() && value == null) {
			return nullDescriptor;
		} else {
			return (Descriptor<?, ?, I>) getDescriptorIndex().getObjectByDefNr(
					1, value);
		}
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
					1003, e, descriptor, getId());
		}

		// make sure it was added
		if (!added && isFailOnDuplicates()) {
			exceptionRegistry.throwException(DescriptorModelException.class,
					1002, descriptor, getId());
		}
	}

	/**
	 * Gets the {@code Index} used to index the different {@code Descriptor}
	 * instances. This method never returns {@code null}, but throws an
	 * exception if the {@code baseIndexedCollectionFactory} isn't defined by
	 * wiring or construction.
	 * 
	 * @return the {@code Index} used to index the different {@code Descriptor}
	 *         instances
	 * 
	 * @throws RuntimeException
	 *             can throw any exception if the
	 *             {@code baseIndexedCollectionFactory} is {@code null}
	 */
	protected IMultipleKeySupport getDescriptorIndex() {
		if (descriptors == null) {
			final IndexKeyDefinition desIdDef = new IndexKeyDefinition(
					Descriptor.class, "getId");
			desIdDef.overrideType(0, getIdClass());
			final IndexKeyDefinition uniqueDesDef = new IndexKeyDefinition(
					Descriptor.class, "getValue");

			// create the descriptors index
			descriptors = baseIndexedCollectionFactory
					.createMultipleKeySupport(desIdDef, uniqueDesDef);
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
}
