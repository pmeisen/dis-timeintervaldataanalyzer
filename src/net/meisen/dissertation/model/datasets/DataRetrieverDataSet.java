package net.meisen.dissertation.model.datasets;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import net.meisen.dissertation.model.dataretriever.BaseDataRetriever;
import net.meisen.dissertation.model.dataretriever.DataCollection;
import net.meisen.dissertation.model.dataretriever.IQueryConfiguration;

public class DataRetrieverDataSet implements IDataSet {
	private final BaseDataRetriever retriever;
	private final IQueryConfiguration query;

	private DataCollection<String> coll = null;

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

	@SuppressWarnings("unchecked")
	protected DataCollection<String> getCollection() {
		if (retriever == null) {
			return null;
		} else if (coll == null) {
			coll = (DataCollection<String>) retriever.retrieve(query);
		}

		return coll;
	}

	protected Collection<String> getNames() {
		final DataCollection<String> coll = getCollection();
		return coll == null ? new ArrayList<String>(0) : coll.getNames();
	}

	@Override
	public Iterator<IDataRecord> iterate() {
		return new DataRetrieverDataSetIterator(getCollection());
	}
}
