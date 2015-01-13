package net.meisen.dissertation.model.descriptors;

import java.lang.reflect.Constructor;
import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.DescriptorModelException;
import net.meisen.dissertation.model.cache.IMetaDataCache;
import net.meisen.dissertation.model.data.OfflineMode;
import net.meisen.dissertation.model.idfactories.IIdsFactory;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.IMultipleKeySupport;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.datarecord.IntervalDataHandling;
import net.meisen.dissertation.model.indexes.datarecord.MetaDataHandling;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.genmisc.types.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
	private final static Logger LOG = LoggerFactory
			.getLogger(DescriptorModel.class);

	private final String id;
	private final String name;
	private final Class<? extends Descriptor> descriptorClass;
	private final IIdsFactory<I> idsFactory;

	private Class<?> descriptorValueClass = null;
	private NullDescriptor<I> nullDescriptor = null;
	private boolean failOnDuplicates = true;
	private boolean supportsNullDescriptor = false;

	private final ReentrantReadWriteLock idxLock;

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	@Autowired
	@Qualifier(DefaultValues.INDEXFACTORY_ID)
	private BaseIndexFactory indexFactory;

	@Autowired
	@Qualifier(DefaultValues.METADATACACHE_ID)
	private IMetaDataCache metaDataCache;

	private IMultipleKeySupport descriptors;
	private OfflineMode offlineMode;

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
	 * @param indexFactory
	 *            the {@code IndexFactory} which determines what index to be
	 *            used for the {@code Descriptor}
	 */
	public DescriptorModel(final String id, final String name,
			final Class<? extends Descriptor> descriptorClass,
			final IIdsFactory<I> idsFactory, final BaseIndexFactory indexFactory) {
		this.idxLock = new ReentrantReadWriteLock();

		this.id = id;
		this.name = name;
		this.descriptorClass = descriptorClass;
		this.idsFactory = idsFactory;
		this.indexFactory = indexFactory;

		// set default offline mode
		setOfflineMode(null);
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
	 * Adds the descriptor with the specified {@code id} and {@code value} to
	 * {@code this}. If a descriptor with the same values is already added the
	 * descriptor of {@code this} is returned, otherwise one is created.
	 * 
	 * @param id
	 *            the identifier of the descriptor to be added
	 * @param value
	 *            the value of the descriptor
	 * 
	 * @return the added descriptor or the an equal descriptor already added to
	 *         the model
	 */
	public <D> Descriptor<D, ?, I> addDescriptor(final I id, final D value) {
		Descriptor descriptor;
		if (value == null) {
			if (!createdNullDescriptor()) {
				nullDescriptor = new NullDescriptor<I>(this, id);
			} else if (!Objects.equals(nullDescriptor.getId(), id)) {
				exceptionRegistry.throwException(
						DescriptorModelException.class, 1008, id, null,
						nullDescriptor);
			}

			descriptor = getNullDescriptor();
		} else if ((descriptor = getDescriptor(id)) == null) {
			descriptor = createDescriptor(id, value);
		} else if (!Objects.equals(descriptor.getValue(), value)) {
			exceptionRegistry.throwException(DescriptorModelException.class,
					1008, id, value, descriptor);
		}

		@SuppressWarnings("unchecked")
		final Descriptor<D, ?, I> typedDescriptor = (Descriptor<D, ?, I>) descriptor;
		return typedDescriptor;
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

		if (values == null) {
			return descriptors;
		} else {
			for (final Object value : values) {
				descriptors.add(createDescriptor(value));
			}

			return descriptors;
		}
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
	@SuppressWarnings("unchecked")
	public <D> Descriptor<D, ?, I> createDescriptor(final D value) {
		Descriptor<D, ?, I> descriptor;

		if (value == null) {
			descriptor = (Descriptor<D, ?, I>) getNullDescriptor();
		} else if ((descriptor = (Descriptor<D, ?, I>) getDescriptorByValue(value)) == null) {
			descriptor = createDescriptor(idsFactory.getId(), value);
		} else if (isFailOnDuplicates()) {
			exceptionRegistry.throwException(DescriptorModelException.class,
					1002, value, getId());
		}

		return descriptor;
	}

	/**
	 * Creates a new {@code Descriptor} instance with the specified {@code id}
	 * and the specified {@code value}. If a descriptor with the specified
	 * {@code id}
	 * 
	 * @param id
	 *            the identifier of the descriptor to be created
	 * @param value
	 *            the value of the descriptor to be created
	 * 
	 * @return the created descriptor or if a descriptor with the specified id
	 *         already exists, the equal descriptor (i.e. same id and same
	 *         value)
	 * 
	 * @throws DescriptorModelException
	 *             if the descriptor cannot be created or if a different
	 *             descriptor (with the id or value) already exists
	 */
	protected <D> Descriptor<D, ?, I> createDescriptor(final I id, final D value)
			throws DescriptorModelException {
		final Class<?> valueType = value.getClass();

		// get the constructor
		final Constructor<? extends Descriptor> constructor = findConstructor(
				descriptorClass, valueType);

		// create the instance and assign the id and a value
		Descriptor<D, ?, I> descriptor;
		try {
			@SuppressWarnings("unchecked")
			final Descriptor<D, ?, I> d = constructor.newInstance(this, id,
					value);
			descriptor = d;
		} catch (final Exception e) {
			exceptionRegistry.throwException(DescriptorModelException.class,
					1000, e, descriptorClass.getName());
			return null;
		}

		// add the descriptor and return it
		if (!addDescriptor(descriptor)) {

			@SuppressWarnings("unchecked")
			final Descriptor<D, ?, I> idxDescriptor = (Descriptor<D, ?, I>) getDescriptor(id);

			// make sure the indexed and the descriptor are equal
			if (Objects.equals(idxDescriptor, descriptor)) {
				descriptor = idxDescriptor;
			} else {
				exceptionRegistry.throwException(
						DescriptorModelException.class, 1007, descriptor,
						idxDescriptor);
				return null;
			}
		}

		// inform the cache about the new descriptor
		cacheDescriptor(descriptor);

		return descriptor;
	}

	/**
	 * Gets the amount of descriptors (without a possible {@code NullDescriptor}
	 * ) in this {@code DescriptorModel}. If descriptors are needed anyways it
	 * makes more sense to get those using {@code #getDescriptors()} and count
	 * the retrieved {@code Collection}.
	 * 
	 * @return the amount of descriptors
	 */
	public int size() {
		idxLock.readLock().lock();
		try {
			return getDescriptorIndex().size();
		} finally {
			idxLock.readLock().unlock();
		}
	}

	/**
	 * Gets the amount of descriptors (with a possible {@code NullDescriptor} )
	 * in this {@code DescriptorModel}. If descriptors are needed anyways it
	 * makes more sense to get those using {@code #getAllDescriptors()} and
	 * count the retrieved {@code Collection}.
	 * 
	 * @return the amount of descriptors
	 */
	public int sizeAll() {
		return getAllDescriptors().size();
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
		if (!createdNullDescriptor()) {
			nullDescriptor = new NullDescriptor<I>(this, idsFactory.getId());
			cacheDescriptor(nullDescriptor);
		}

		return nullDescriptor;
	}

	/**
	 * Caches the {@code Descriptor} if a cache is available.
	 * 
	 * @param desc
	 *            the {@code Descriptor} to be cached
	 */
	protected void cacheDescriptor(final Descriptor<?, ?, ?> desc) {
		if (metaDataCache != null) {
			metaDataCache.cacheDescriptor(desc);
		}
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
	 * Checks if the {@code NullDescriptor} was already created. This method
	 * does not check if a {@code NullDescriptor} is supported, it just checks
	 * if it was created already.
	 * 
	 * @return {@code true} if the {@code NullDescriptor} for this
	 *         {@code DescriptorModel} was created, otherwise {@code false}
	 * 
	 * @see #supportsNullDescriptor()
	 */
	protected boolean createdNullDescriptor() {
		return nullDescriptor != null;
	}

	/**
	 * Gets the {@code Descriptors} of {@code this} model. The list does
	 * <b>not</b> include a {@code NullDescriptor} if one is used by the model.
	 * To get all descriptors use {@link #getAllDescriptors()}.
	 * 
	 * @return all the {@code Descriptors} of {@code this} model
	 */
	@SuppressWarnings("unchecked")
	public Collection<Descriptor<?, ?, I>> getDescriptors() {
		idxLock.readLock().lock();
		try {
			return (Collection<Descriptor<?, ?, I>>) getDescriptorIndex()
					.getAll();
		} finally {
			idxLock.readLock().unlock();
		}
	}

	/**
	 * Gets all the {@code Descriptors} of {@code this} model.
	 * 
	 * @return all the {@code Descriptors} of {@code this} model
	 */
	public Collection<Descriptor<?, ?, I>> getAllDescriptors() {

		if (supportsNullDescriptor()) {

			// create a list with the null descriptor
			final List<Descriptor<?, ?, I>> list = new ArrayList<Descriptor<?, ?, I>>();
			list.add(getNullDescriptor());
			list.addAll(getDescriptors());

			return list;
		} else {
			return getDescriptors();
		}
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
	public Descriptor<?, ?, I> getDescriptor(final I id) {

		if (id == null) {
			exceptionRegistry.throwException(DescriptorModelException.class,
					1005);
			return null;
		} else if (supportsNullDescriptor() && createdNullDescriptor()
				&& id.equals(getNullDescriptorId())) {
			return getNullDescriptor();
		} else {
			return getDescriptorByDefNr(0, id);
		}
	}

	/**
	 * Retrieves the specified descriptor.
	 * 
	 * @param keyDefNr
	 *            the number of the key of the index, which defines the values
	 * @param values
	 *            the values to be mapped
	 * 
	 * @return the descriptor found or {@code null} if none was found
	 */
	@SuppressWarnings("unchecked")
	protected Descriptor<?, ?, I> getDescriptorByDefNr(final int keyDefNr,
			final Object... values) {
		idxLock.readLock().lock();
		try {
			return (Descriptor<?, ?, I>) getDescriptorIndex().getObjectByDefNr(
					keyDefNr, values);
		} finally {
			idxLock.readLock().unlock();
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
	public Descriptor<?, ?, I> getDescriptorByValue(final Object value) {

		if (supportsNullDescriptor() && value == null) {
			return getNullDescriptor();
		} else if (value == null) {
			exceptionRegistry.throwException(DescriptorModelException.class,
					1004, getId());
			return null;
		} else if (getValueType().equals(Object.class)) {
			if (value instanceof String) {
				return getDescriptorByString((String) value);
			} else if (getValueType().isAssignableFrom(value.getClass())) {
				return getDescriptorByDefNr(1, value);
			} else {
				return null;
			}
		} else if (getValueType().isAssignableFrom(value.getClass())) {
			return getDescriptorByDefNr(1, value);
		} else if (value instanceof String) {
			return getDescriptorByString((String) value);
		} else {
			return null;
		}
	}

	/**
	 * Gets the descriptor for the specified value. If the descriptor is not
	 * found, a strategy based on the passed {@code MetaDataHandling} is
	 * applied.
	 * 
	 * @param value
	 *            the value of the {@code Descriptor} to be returned
	 * @param handling
	 *            the handling strategy
	 * 
	 * @return the {@code Descriptor} with the specified {@code value}
	 * 
	 * @see MetaDataHandling
	 */
	public Descriptor<?, ?, I> getDescriptorByValue(final Object value,
			final MetaDataHandling handling) {

		// get the descriptor just by value
		Descriptor<?, ?, I> desc = getDescriptorByValue(value);

		// if not find handle it according to the defined handling
		if (desc == null) {
			if (MetaDataHandling.CREATEDESCRIPTOR.equals(handling)) {
				desc = createDescriptor(value);
			} else if (MetaDataHandling.FAILONERROR.equals(handling)) {
				exceptionRegistry.throwException(
						DescriptorModelException.class, 1006, value);
			} else if (MetaDataHandling.HANDLEASNULL.equals(handling)) {
				desc = getNullDescriptor();
			}
		}

		return desc;
	}

	/**
	 * Gets the {@code Descriptor} having the {@code value} defined as unique
	 * string.
	 * 
	 * @param value
	 *            the unique string of the {@code Descriptor} to be returned
	 * @return the {@code Descriptor} with the specified unique string, if no
	 *         {@code Descriptor} can be found {@code null} is returned
	 * 
	 * @see Descriptor#getUniqueString()
	 */
	public Descriptor<?, ?, I> getDescriptorByString(final String value) {

		if (value == null) {
			if (supportsNullDescriptor()) {
				return getNullDescriptor();
			} else {
				return null;
			}
		} else {
			return getDescriptorByDefNr(2, value);
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
	 * 
	 * @return {@code true} if all descriptors were added, otherwise
	 *         {@code false}
	 */
	protected boolean addDescriptors(final Collection<Descriptor> descriptors) {

		boolean added = true;
		if (descriptors != null) {
			for (final Descriptor descriptor : descriptors) {
				added = addDescriptor(descriptor) && added;
			}
		}

		return added;
	}

	/**
	 * Adds the {@code Descriptor} to the index of {@code this} model.
	 * 
	 * @param descriptor
	 *            the descriptor to be added
	 * 
	 * @return {@code true} if the {@code Descriptor} was added, otherwise
	 *         {@code false}
	 * 
	 * @throws DescriptorModelException
	 *             if duplicates aren't allowed and a {@code Descriptor} was
	 *             already indexed
	 */
	protected boolean addDescriptor(final Descriptor descriptor)
			throws DescriptorModelException {
		boolean added = false;

		idxLock.writeLock().lock();
		try {
			added = getDescriptorIndex().addObject(descriptor);

			// mark the id as used if added
			if (added) {

				@SuppressWarnings("unchecked")
				final I id = (I) descriptor.getId();
				idsFactory.setIdAsUsed(id);
			}
		} catch (final Exception e) {
			exceptionRegistry.throwException(DescriptorModelException.class,
					1003, e, descriptor.getUniqueString(), getId());
		} finally {
			idxLock.writeLock().unlock();
		}

		// if it wasn't added, than we have a duplicate
		if (!added && isFailOnDuplicates()) {
			exceptionRegistry.throwException(DescriptorModelException.class,
					1002, descriptor.getUniqueString(), getId());
		}

		return added;
	}

	/**
	 * Gets the {@code Index} used to index the different {@code Descriptor}
	 * instances. This method never returns {@code null}, but throws an
	 * exception if the {@code indexFactory} isn't defined by wiring or
	 * construction.
	 * 
	 * @return the {@code Index} used to index the different {@code Descriptor}
	 *         instances
	 * 
	 * @throws RuntimeException
	 *             can throw any exception if the {@code indexFactory} is
	 *             {@code null}
	 */
	protected IMultipleKeySupport getDescriptorIndex() {
		if (descriptors == null) {
			final IndexKeyDefinition desIdDef = new IndexKeyDefinition(
					Descriptor.class, "getId");
			desIdDef.overrideType(0, getIdClass());
			final IndexKeyDefinition valueDef = new IndexKeyDefinition(
					Descriptor.class, "getValue");
			valueDef.overrideType(0, getValueType());
			final IndexKeyDefinition stringDef = new IndexKeyDefinition(
					Descriptor.class, "getUniqueString");

			// create the descriptors index
			descriptors = indexFactory.createMultipleKeySupport(desIdDef,
					valueDef, stringDef);
		}

		return descriptors;
	}

	/**
	 * Gets the type of the value of the {@code Descriptor} managed by
	 * {@code this}.
	 * 
	 * @return the type of the value of the {@code Descriptor} managed by
	 *         {@code this}
	 */
	public Class<?> getValueType() {

		// determine the class if not known
		if (descriptorValueClass == null) {
			try {
				descriptorValueClass = (Class<?>) ((ParameterizedType) descriptorClass
						.getGenericSuperclass()).getActualTypeArguments()[0];
			} catch (final Exception e) {
				if (LOG.isWarnEnabled()) {
					LOG.warn("Cannot determine the value-type of descriptor-class '"
							+ descriptorClass.getName() + "'.");
				}

				descriptorValueClass = Object.class;
			}
		}

		return descriptorValueClass;
	}

	private Constructor<? extends Descriptor> findConstructor(
			final Class<? extends Descriptor> clazz, final Class<?> valueType) {

		final List<Class<?>[]> ctors = new ArrayList<Class<?>[]>();
		ctors.add(new Class[] { getIdClass(), valueType });
		ctors.add(new Class[] { Object.class, valueType });
		ctors.add(new Class[] { Object.class, Object.class });

		Constructor<? extends Descriptor> constructor = null;
		for (Class<?>[] ctor : ctors) {
			try {
				constructor = clazz.getConstructor(DescriptorModel.class,
						ctor[0], ctor[1]);
			} catch (final NoSuchMethodException e) {
				constructor = null;
				continue;
			}

			// we found one
			break;
		}

		// if not throw the exception
		if (constructor == null) {
			exceptionRegistry.throwException(DescriptorModelException.class,
					1001, clazz.getName(), idsFactory.getIdClass().getName(),
					valueType.getName());
			return null;
		} else {
			return constructor;
		}
	}

	/**
	 * Gets the {@code OfflineMode} defined for the {@code DescriptorModel}.
	 * 
	 * @return the {@code OfflineMode}
	 * 
	 * @see OfflineMode
	 */
	public OfflineMode getOfflineMode() {
		return offlineMode;
	}

	/**
	 * Sets the {@code OfflineMode} to be used by the {@code DescriptorModel}.
	 * 
	 * @param offlineMode
	 *            the {@code OfflineMode} to be used
	 * 
	 * @see OfflineMode
	 */
	public void setOfflineMode(final OfflineMode offlineMode) {
		this.offlineMode = offlineMode == null ? OfflineMode.find(null)
				: offlineMode;
	}

	/**
	 * Sets the {@code OfflineMode}, i.e. how invalid data retrievers should be
	 * handled.
	 * 
	 * @param mode
	 *            the {@code OfflineMode} to be used
	 * 
	 * @see IntervalDataHandling
	 */
	public void setOfflineModeByString(final String mode) {
		setOfflineMode(OfflineMode.find(mode));
	}

	/**
	 * Casts the specified {@code id} to the type of the identifier of the
	 * model's descriptors.
	 * 
	 * @param id
	 *            the id to be casted
	 * 
	 * @return the casted id
	 */
	@SuppressWarnings("unchecked")
	public I castId(final Object id) {
		return (I) id;
	}

	@Override
	public String toString() {
		idxLock.readLock().lock();
		try {
			return getDescriptorIndex().toString();
		} finally {
			idxLock.readLock().unlock();
		}
	}
}
