package net.meisen.dissertation.impl.datasets;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import net.meisen.dissertation.model.dataretriever.BaseDataRetriever;
import net.meisen.dissertation.model.dataretriever.DataCollection;
import net.meisen.dissertation.model.dataretriever.EmptyDataCollection;
import net.meisen.dissertation.model.dataretriever.IQueryConfiguration;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.datasets.IDataSet;

/**
 * {@code DataSet} which is used to retrieve the data available from a
 * {@code DataRetriever}.
 * 
 * @author pmeisen
 * 
 * @see IDataSet
 * @see BaseDataRetriever
 */
public class DataRetrieverDataSet implements IDataSet {
	private final static DataCollection<String> EMPTYCOLL = new EmptyDataCollection<String>();

	private final BaseDataRetriever retriever;
	private final IQueryConfiguration query;

	private DataCollection<String> coll = null;

	/**
	 * Constructor of a {@code DataSet} which defines the {@code DataRetriever}
	 * and the {@code QueryConfiguration} used to retrieve the data.
	 * 
	 * @param retriever
	 *            the {@code DataRetriever} used to retrieve the data from
	 * @param query
	 *            the query to be fired against the retriever
	 */
	public DataRetrieverDataSet(final BaseDataRetriever retriever,
			final IQueryConfiguration query) {
		this.retriever = retriever;
		this.query = query;
	}

	@Override
	public boolean hasNamedValue(final String name) {
		return getNames().contains(name);
	}

	@Override
	public boolean isValidPosition(final int position) {
		return position > 0 && position <= getNames().size();
	}

	/**
	 * Gets the {@code DataCollection} which can iterate over all the data of
	 * the {@code DataSet}.
	 * 
	 * @return the {@code DataCollection} of the underlying
	 *         {@code DataRetriever}
	 */
	@SuppressWarnings("unchecked")
	protected DataCollection<String> getCollection() {
		if (retriever == null) {
			return EMPTYCOLL;
		} else if (coll == null) {
			coll = (DataCollection<String>) retriever.retrieve(query);
		}

		return coll;
	}

	/**
	 * Gets the names of the data available within the {@code DataCollection}.
	 * 
	 * @return the names of the data available within the {@code DataCollection}
	 */
	protected Collection<String> getNames() {
		final DataCollection<String> coll = getCollection();
		return coll == null ? new ArrayList<String>(0) : coll.getNames();
	}

	@Override
	public Iterator<IDataRecord> iterator() {
		return new DataRetrieverDataSetIterator(getCollection());
	}

	@Override
	public boolean isOfflineAvailable() {
		return false;
	}
}
