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
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.datastructure.MetaStructureEntry;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.IIndexedCollection;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.datarecord.slices.IndexDimensionSlice;
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

	private MetaDataHandling metaDataHandling;
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
				IndexDimensionSlice.class, "getId");
		indexKeyDef.overrideType(0, model.getIdClass());
		this.index = indexFactory.create(indexKeyDef);

		// set the default value
		setMetaDataHandling(null);

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
	public void index(final int recId, final IDataRecord rec) {
		if (rec == null) {
			return;
		}

		// get the id
		final I id = getIdFromRecord(rec);

		// create or get the slices
		final IndexDimensionSlice<I> slice = getSliceById(id);
		if (slice == null) {
			index.addObject(new IndexDimensionSlice<I>(id, indexFactory, recId));
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
	public IndexDimensionSlice<I> getSliceById(final I valueId) {
		return (IndexDimensionSlice<I>) index.getObject(valueId);
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
	public IndexDimensionSlice<I> getSliceByValue(final Object value) {
		final I id = getIdForValue(value);
		return getSliceById(id);
	}

	/**
	 * Gets the slices of the {@code MetaIndexDimension}.
	 * 
	 * @return the slices of the {@code MetaIndexDimension}
	 */
	@SuppressWarnings("unchecked")
	public IndexDimensionSlice<I>[] getSlices() {
		final Collection<?> all = index.getAll();
		return (IndexDimensionSlice<I>[]) index.getAll().toArray(
				new IndexDimensionSlice<?>[all.size()]);
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
	 */
	public int[] getByValue(final Object value) {
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
		final IndexDimensionSlice<I> slice = getSliceById(id);
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
	 * @throws NullPointerException
	 *             if the value to be indexed cannot be found within the
	 *             {@code DescriptorModel} and
	 *             {@link MetaDataHandling#FAILONERROR} is selected as
	 *             handling-strategy
	 * @throws DescriptorModelException
	 *             if the {@code DescriptorModel} doesn't support {@code null}
	 *             values
	 * 
	 * @see MetaDataHandling
	 */
	protected I getIdForValue(final Object value) {
		return getIdForValue(value, getMetaDataHandling());
	}

	/**
	 * Gets the id of the value using the specified {@code MetaDataHandling}. If
	 * the {@code handling} is {@code null} the defined handling of {@code this}
	 * will be used (see {@link #getMetaDataHandling()}).
	 * 
	 * @param value
	 *            the value to get the id for
	 * @param handling
	 *            the {@code MetaDataHandling} if the value cannot be resolved
	 * 
	 * @return the identifier
	 */
	protected I getIdForValue(final Object value,
			final MetaDataHandling handling) {
		Descriptor<?, ?, I> desc = model.getDescriptorByValue(value);
		if (desc == null) {
			final MetaDataHandling metaDataHandling = handling == null ? getMetaDataHandling()
					: handling;

			if (MetaDataHandling.CREATEDESCRIPTOR.equals(metaDataHandling)) {
				desc = model.createDescriptor(value);
			} else if (MetaDataHandling.FAILONERROR.equals(metaDataHandling)) {
				throw new NullPointerException("The value '" + value
						+ "' doesn't have any descriptor.");
			} else if (MetaDataHandling.HANDLEASNULL.equals(metaDataHandling)) {
				desc = model.getNullDescriptor();
			}
		}

		return desc.getId();
	}

	/**
	 * Determines the id of the {@code DataRecord} to be handled by this
	 * dimension.
	 * 
	 * @param record
	 *            the {@code DataRecord} to determine the id for, cannot be
	 *            {@code null}
	 * 
	 * @return the identifier, which cannot be {@code null}
	 * 
	 * @throws NullPointerException
	 *             if the {@code record} was {@code null}, or if the value to be
	 *             indexed cannot be found within the {@code DescriptorModel}
	 *             and {@link MetaDataHandling#FAILONERROR} is selected as
	 *             handling-strategy
	 * @throws DescriptorModelException
	 *             if the {@code DescriptorModel} doesn't support {@code null}
	 *             values
	 * 
	 * @see MetaDataHandling
	 */
	protected I getIdFromRecord(final IDataRecord record)
			throws DescriptorModelException, NullPointerException {
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

	/**
	 * Get the defined {@code MetaDataHandling}.
	 * 
	 * @return the defined {@code MetaDataHandling}
	 * 
	 * @see MetaDataHandling
	 */
	public MetaDataHandling getMetaDataHandling() {
		return metaDataHandling;
	}

	/**
	 * Set the {@code MetaDataHandling}.
	 * 
	 * @param metaDataHandling
	 *            the {@code MetaDataHandling}
	 * 
	 * @see MetaDataHandling
	 */
	public void setMetaDataHandling(final MetaDataHandling metaDataHandling) {
		this.metaDataHandling = metaDataHandling == null ? MetaDataHandling
				.find(null) : metaDataHandling;
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
		for (final IndexDimensionSlice<I> slice : getSlices()) {
			slice.optimize();
		}
	}

	@Override
	public void save(final BasePersistor persistor) {

		for (final IndexDimensionSlice<I> slice : getSlices()) {
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
		final I id = getIdForValue(value, MetaDataHandling.CREATEDESCRIPTOR);

		// check if the slice is already indexed
		if (index.getObject(id) != null) {
			throw new ForwardedRuntimeException(PersistorException.class, 1004,
					"The identifier '" + id + "' ('" + value
							+ "') already exists.");
		}

		// create the slice
		final IndexDimensionSlice<I> slice = new IndexDimensionSlice<I>(id,
				indexFactory);

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
