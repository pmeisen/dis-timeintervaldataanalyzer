package net.meisen.dissertation.model.indexes.datarecord;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.UUID;

import net.meisen.dissertation.exceptions.PersistorException;
import net.meisen.dissertation.model.cache.IDataRecordCache;
import net.meisen.dissertation.model.data.TidaModel;
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

	private Group persistentGroup = null;

	/**
	 * Default constructor, creating an instance for the specified {@code model}
	 * .
	 * 
	 * @param model
	 *            the model the index is created for
	 */
	public DataRecordIndex(final TidaModel model) {
		this(model.getDataRecordCache());
	}

	/**
	 * Constructor to create an index for the specified {@code recordCache}.
	 * 
	 * @param recordCache
	 *            the cache to be used for the index
	 */
	public DataRecordIndex(final IDataRecordCache recordCache) {
		this.recordCache = recordCache;
	}

	@Override
	public void index(final ProcessedDataRecord record) {
		this.recordCache.cache(record);
	}

	/**
	 * Gets the names of the records to be retrieved from the index.
	 * 
	 * @return the names of the records to be retrieved from the index
	 */
	public String[] getNames() {
		return this.recordCache.getNames();
	}

	/**
	 * Gets the types of the records to be retrieved from the index.
	 * 
	 * @return the names of the records to be retrieved from the index
	 */
	public Class<?>[] getTypes() {
		return this.recordCache.getTypes();
	}

	/**
	 * Gets the record with the specified {@code recordId} from the index.
	 * 
	 * @param recordId
	 *            the identifier to get the record for
	 * 
	 * @return the record with the specified {@code recordId}
	 */
	public Object[] get(final int recordId) {
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
			persistor.writeInt(out, getNames().length);
			persistor.writeInt(out, recordCache.size());

			/*
			 * write all the records
			 */
			while (it.hasNext()) {
				final int recordId = it.next();
				persistor.writeInt(out, recordId);

				final Object[] record = this.get(recordId);
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
				if (this.recordCache.get(recId) == null) {
					this.recordCache.cache(recId, record);
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
