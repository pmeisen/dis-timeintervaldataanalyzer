package net.meisen.dissertation.model.indexes.datarecord;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Collection;
import java.util.UUID;

import net.meisen.dissertation.exceptions.DescriptorModelException;
import net.meisen.dissertation.exceptions.PersistorException;
import net.meisen.dissertation.model.datastructure.MetaStructureEntry;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.IIndexedCollection;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.datarecord.slices.Slice;
import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.Group;
import net.meisen.dissertation.model.persistence.Identifier;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A dimension of a {@code MetaIndex}. A dimension is an instance of meta-data,
 * which are logically grouped. A dimension could be e.g. family which consists
 * of the different family members, or numbers which consists of different
 * numbers between 1 and 10.
 * 
 * @author pmeisen
 * 
 * @param <I>
 *            the type of the indexed values
 */
public class MetaIndexDimension<I> implements IDataRecordIndex {
	private final static String EXTENSION = ".slice";
	private final static Logger LOG = LoggerFactory
			.getLogger(MetaIndexDimension.class);

	private final DescriptorModel<I> model;
	private final MetaStructureEntry metaEntry;

	private final BaseIndexFactory indexFactory;
	private final IIndexedCollection index;

	private Group persistentGroup = null;

	/**
	 * Constructor used to create a {@code MetaIndexDimension} for the specified
	 * {@code metaEntry}, the specified {@code model}
	 * 
	 * @param metaEntry
	 *            the {@code MetaStructureEntry} which defines this
	 *            {@code MetaIndexDimension}
	 * @param model
	 *            the {@code DescriptorModel} referred by the
	 *            {@code MetaStructureEntry}
	 * @param indexFactory
	 *            the {@code IndexFactory} used to decide which index should be
	 *            used for referring to the different {@code IndexBitmapSlice}
	 *            instances
	 * 
	 * @see MetaStructureEntry
	 * @see DescriptorModel
	 */
	public MetaIndexDimension(final MetaStructureEntry metaEntry,
			final DescriptorModel<I> model, final BaseIndexFactory indexFactory) {
		if (model == null) {
			throw new NullPointerException("The model cannot be null.");
		} else if (metaEntry == null) {
			throw new NullPointerException("The metaEntry cannot be null.");
		} else if (!model.getId().equals(metaEntry.getDescriptorModel())) {
			throw new IllegalArgumentException("The model identifier '"
					+ model.getId()
					+ "' must be equal to the entries descriptorModel '"
					+ metaEntry.getDescriptorModel() + "'.");
		} else if (LOG.isTraceEnabled()) {
			LOG.trace("Creating MetaIndexDimension for '"
					+ metaEntry.getDescriptorModel() + "'...");
		}

		// set the values
		this.metaEntry = metaEntry;
		this.model = model;
		this.indexFactory = indexFactory;

		// create an index to handle the different values for a descriptor
		final IndexKeyDefinition indexKeyDef = new IndexKeyDefinition(
				Slice.class, "getId");
		indexKeyDef.overrideType(0, model.getIdClass());
		this.index = indexFactory.create(indexKeyDef);

		// log the successful creation
		if (LOG.isTraceEnabled()) {
			LOG.trace("Created MetaIndexDimension for '"
					+ metaEntry.getDescriptorModel() + "' with index '"
					+ this.index.getClass().getName()
					+ "' for identifiers of model of type '"
					+ model.getIdClass().getName() + "'.");
		}
	}

	/**
	 * Gets the identifier of the model this {@code MetaIndexDimension} is used
	 * for.
	 * 
	 * @return the identifier of the model this {@code MetaIndexDimension} is
	 *         used for
	 */
	public String getModelId() {
		return model.getId();
	}

	/**
	 * Gets the {@code DescriptorModel} of this {@code MetaIndexDimension}.
	 * 
	 * @return the {@code DescriptorModel} of this {@code MetaIndexDimension}
	 */
	public DescriptorModel<I> getModel() {
		return model;
	}

	@Override
	public void index(final int recId, final ProcessedDataRecord rec) {
		if (rec == null) {
			return;
		}

		// get the id
		@SuppressWarnings("unchecked")
		final Descriptor<?, ?, I> desc = (Descriptor<?, ?, I>) rec
				.getDescriptor(metaEntry);
		final I id = desc.getId();

		// create or get the slices
		final Slice<I> slice = getSliceById(id);
		if (slice == null) {
			index.addObject(new Slice<I>(id, indexFactory, recId));
		} else {
			slice.set(recId);
		}
	}

	/**
	 * Gets a slice of the dimension, i.e. a bitmap which defines which records
	 * have the value of the specified slice set (i.e. {@code 1}) and which
	 * don't (i.e. {@code 0}).
	 * 
	 * @param valueId
	 *            the identifier of the value, i.e. the identifier of the value
	 *            of the dimension to retrieve the information for
	 * @return a bitmap with the identifiers of the records set to {@code 1} if
	 *         and only if the record's value is referred by the specified
	 *         {@code valueId}
	 */
	@SuppressWarnings("unchecked")
	public Slice<I> getSliceById(final I valueId) {
		return (Slice<I>) index.getObject(valueId);
	}

	/**
	 * Gets a slice of the dimension, i.e. a bitmap which defines which records
	 * have the value of the specified slice set (i.e. {@code 1}) and which
	 * don't (i.e. {@code 0}).
	 * 
	 * @param value
	 *            the value, i.e. the value of the dimension to retrieve the
	 *            information for
	 * @return a bitmap with the identifiers of the records set to {@code 1} if
	 *         and only if the record's value is equal to the {@code value}
	 */
	public Slice<I> getSliceByValue(final Object value) {
		final I id = getIdForValue(value);
		return getSliceById(id);
	}

	/**
	 * Gets the slices of the {@code MetaIndexDimension}.
	 * 
	 * @return the slices of the {@code MetaIndexDimension}
	 */
	@SuppressWarnings("unchecked")
	public Slice<I>[] getSlices() {
		final Collection<?> all = index.getAll();
		return (Slice<I>[]) index.getAll().toArray(new Slice<?>[all.size()]);
	}

	/**
	 * Gets the amounts of slices, i.e. different values within the dimension.
	 * 
	 * @return the amounts of slices
	 */
	public int getAmountOfSlices() {
		return index.size();
	}

	/**
	 * Gets an array of the records (by identifier) which have the specified
	 * {@code value} set.
	 * 
	 * @param value
	 *            the value the records should have set
	 * 
	 * @return an array of the records (by identifier) which have the specified
	 *         {@code value} set
	 * 
	 * @throws DescriptorModelException
	 *             if the {@code DescriptorModel} doesn't support {@code null}
	 *             values
	 * @throws NullPointerException
	 *             if no descriptor for the specified value exists
	 */
	public int[] getByValue(final Object value)
			throws DescriptorModelException, NullPointerException {
		final I id = getIdForValue(value);
		return getById(id);
	}

	/**
	 * Gets an array of the records (by identifier) which have the specified
	 * value - referred by {@code id} - set.
	 * 
	 * @param id
	 *            the id of the value which the records should have set
	 * 
	 * @return an array of the records (by identifier) which have the specified
	 *         value - referred by {@code id} - set
	 */
	public int[] getById(final I id) {
		final Slice<I> slice = getSliceById(id);
		if (slice == null) {
			return new int[0];
		} else {
			return slice.get();
		}
	}

	/**
	 * Determines the id of the value to be handled by this dimension.
	 * 
	 * @param value
	 *            the value to determine the id for
	 * 
	 * @return the identifier, which cannot be {@code null}
	 * 
	 * @throws DescriptorModelException
	 *             if the {@code DescriptorModel} doesn't support {@code null}
	 *             values
	 * @throws NullPointerException
	 *             if no descriptor for the specified value exists
	 */
	protected I getIdForValue(final Object value)
			throws DescriptorModelException, NullPointerException {
		final Descriptor<?, ?, I> desc = model.getDescriptorByValue(value);
		return desc.getId();
	}

	/**
	 * Gets the class of the {@ode Index} using to index the different
	 * slices of the {@code MetaIndexDimension}.
	 * 
	 * @return the class of the {@ode Index}
	 */
	protected Class<? extends IIndexedCollection> getIndexClass() {
		return index.getClass();
	}

	@Override
	public void optimize() {
		for (final Slice<I> slice : getSlices()) {
			slice.optimize();
		}
	}

	@Override
	public void save(final BasePersistor persistor) {

		for (final Slice<I> slice : getSlices()) {
			if (slice == null) {
				continue;
			}

			// if we have a slice persist it
			final Descriptor<?, ?, I> desc = model.getDescriptor(slice.getId());
			final String fileName = UUID.randomUUID().toString() + EXTENSION;
			final Identifier id = new Identifier(fileName, persistentGroup);
			id.setComment(desc.toString());

			final OutputStream out = persistor.openForWrite(id);
			try {
				persistor.writeObject(out, desc.getValue());
				slice.getBitmap().serialize(new DataOutputStream(out));
			} catch (final IOException e) {
				throw new ForwardedRuntimeException(PersistorException.class,
						1003, e, e.getMessage());
			}

			persistor.close(id);
		}
	}

	@Override
	public void load(final BasePersistor persistor,
			final Identifier identifier, final InputStream inputStream) {

		// read the value
		final Object value;
		try {
			value = persistor.readObject(inputStream);
		} catch (final Exception e) {
			throw new ForwardedRuntimeException(PersistorException.class, 1004,
					e, e.getMessage());
		}

		// get the identifier for the value
		final Descriptor<?, ?, I> desc = model.getDescriptorByValue(value,
				MetaDataHandling.CREATEDESCRIPTOR);
		final I id = desc.getId();

		// check if the slice is already indexed
		if (index.getObject(id) != null) {
			throw new ForwardedRuntimeException(PersistorException.class, 1004,
					"The identifier '" + id + "' ('" + value
							+ "') already exists.");
		}

		// create the slice
		final Slice<I> slice = new Slice<I>(id, indexFactory);

		// load the slice from the InputStream
		try {
			slice.getBitmap().deserialize(new DataInputStream(inputStream));
		} catch (final IOException e) {
			throw new ForwardedRuntimeException(PersistorException.class, 1004,
					e, e.getMessage());
		}

		// add the slice
		index.addObject(slice);
	}

	@Override
	public void isRegistered(final BasePersistor persistor, final Group group) {
		this.persistentGroup = group;
	}

	@Override
	public Group getPersistentGroup() {
		return persistentGroup;
	}
}
