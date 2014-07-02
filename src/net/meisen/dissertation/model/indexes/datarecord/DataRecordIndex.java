package net.meisen.dissertation.model.indexes.datarecord;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.UUID;

import net.meisen.dissertation.exceptions.PersistorException;
import net.meisen.dissertation.model.cache.IDataRecordCache;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.Group;
import net.meisen.dissertation.model.persistence.Identifier;
import net.meisen.dissertation.model.util.IIntIterator;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * Index for the different records added.
 * 
 * @author pmeisen
 * 
 */
public class DataRecordIndex implements IDataRecordIndex {
	private final static String EXTENSION = ".records";

	private final IDataRecordCache recordCache;
	private final IDataRecordFactory recordFactory;

	private Group persistentGroup = null;

	/**
	 * Default constructor, creating an instance for the specified {@code model}
	 * .
	 * 
	 * @param model
	 *            the model the index is created for
	 */
	public DataRecordIndex(final TidaModel model) {
		this(model.getDataRecordCache(), model.getDataRecordFactory());
	}

	/**
	 * Constructor to create an index for the specified {@code recordCache}.
	 * 
	 * @param recordCache
	 *            the cache to be used for the index
	 * @param factory
	 *            the {@code DataRecordFactory} to be used to create the records
	 */
	public DataRecordIndex(final IDataRecordCache recordCache,
			final IDataRecordFactory factory) {
		this.recordCache = recordCache;
		this.recordFactory = factory;
	}

	@Override
	public void index(final ProcessedDataRecord record) {
		this.recordCache.cache(record);
	}

	/**
	 * Gets the meta-information of records indexed.
	 * 
	 * @return the meta-information of records indexed.
	 */
	public IDataRecordMeta getMeta() {
		return this.recordFactory.getMeta();
	}

	/**
	 * Gets the record with the specified {@code recordId} from the index.
	 * 
	 * @param recordId
	 *            the identifier of the record for
	 * 
	 * @return the record with the specified {@code recordId}; {@code null} if
	 *         no record with the specified identifier exists
	 */
	public IDataRecord get(final int recordId) {
		final Object[] rec = getArray(recordId);
		if (rec == null) {
			return null;
		} else {
			return recordFactory.create(rec);
		}
	}

	/**
	 * Gets the object-array representing the values of the record with the
	 * specified {@code recordId} from the index.
	 * 
	 * @param recordId
	 *            the identifier of the record for
	 * 
	 * @return the object-array representing the values of the record with the
	 *         specified {@code recordId}; {@code null} if no record with the
	 *         specified identifier exists
	 */
	public Object[] getArray(final int recordId) {
		return this.recordCache.get(recordId);
	}

	@Override
	public void optimize() {
		// nothing to be optimized
	}

	@Override
	public void isRegistered(final BasePersistor persistor, final Group group) {
		this.persistentGroup = group;
	}

	@Override
	public void save(final BasePersistor persistor)
			throws ForwardedRuntimeException {

		final String id = UUID.randomUUID().toString() + EXTENSION;
		final Identifier identifier = new Identifier(id, persistentGroup);
		identifier.setComment("Records");

		final OutputStream out = persistor.openForWrite(identifier);
		final IIntIterator it = this.recordCache.intIterator();
		try {

			/*
			 * write the amount of entries per record, as well as the amount of
			 * records
			 */
			persistor.writeInt(out, getMeta().getDataTypes().length);
			persistor.writeInt(out, recordCache.size());

			/*
			 * write all the records
			 */
			while (it.hasNext()) {
				final int recordId = it.next();
				persistor.writeInt(out, recordId);

				final Object[] record = recordCache.get(recordId);
				for (final Object rec : record) {
					persistor.writeObject(out, rec);
				}
			}
		} catch (final IOException e) {
			throw new ForwardedRuntimeException(PersistorException.class, 1003,
					e, e.getMessage());
		} finally {
			persistor.close(identifier);
		}
	}

	@Override
	public void load(final BasePersistor persistor, final Identifier id,
			final InputStream in) throws ForwardedRuntimeException {

		// get the InputStream
		try {
			final int recSize = persistor.readInt(in);
			final int amount = persistor.readInt(in);

			for (int i = 0; i < amount; i++) {
				final int recId = persistor.readInt(in);
				final Object[] record = new Object[recSize];

				for (int k = 0; k < recSize; k++) {
					record[k] = persistor.readObject(in);
				}

				// cache the stuff
				if (recordCache.get(recId) == null) {
					recordCache.cache(recId, record);
				} else {
					throw new ForwardedRuntimeException(
							PersistorException.class, 1004,
							"A record with identifier '" + recId
									+ "' already exists.");
				}
			}
		} catch (final Exception e) {
			if (e instanceof ForwardedRuntimeException) {
				throw (ForwardedRuntimeException) e;
			} else {
				throw new ForwardedRuntimeException(PersistorException.class,
						1004, e, e.getMessage());
			}
		}

	}

	@Override
	public Group getPersistentGroup() {
		return persistentGroup;
	}
}
