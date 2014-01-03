package net.meisen.dissertation.data.impl.dataretriever;

import java.util.Iterator;

import net.meisen.dissertation.models.impl.dataretriever.BaseDataRetriever;
import net.meisen.dissertation.models.impl.dataretriever.DataCollection;
import net.meisen.dissertation.models.impl.dataretriever.DataRecord;
import net.meisen.dissertation.models.impl.dataretriever.IDataRetrieverConfiguration;
import net.meisen.dissertation.models.impl.dataretriever.IQueryConfiguration;

public class RandomDataRetriever extends BaseDataRetriever {

	public RandomDataRetriever(final IDataRetrieverConfiguration config) {
		super(config);
	}

	@Override
	public DataCollection<?> retrieve(
			final IQueryConfiguration queryConfiguration) {

		return new DataCollection<String>(new String[] { "A" }) {

			@Override
			public Iterator<DataRecord<String>> open() {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			public void close() {
				// TODO Auto-generated method stub

			}
		};
	}
}
