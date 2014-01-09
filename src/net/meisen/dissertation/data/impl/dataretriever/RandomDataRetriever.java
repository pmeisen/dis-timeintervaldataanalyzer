package net.meisen.dissertation.data.impl.dataretriever;

import java.util.Iterator;

import net.meisen.dissertation.models.impl.dataretriever.BaseDataRetriever;
import net.meisen.dissertation.models.impl.dataretriever.DataCollection;
import net.meisen.dissertation.models.impl.dataretriever.DataIterator;
import net.meisen.dissertation.models.impl.dataretriever.DataRecord;
import net.meisen.dissertation.models.impl.dataretriever.IDataRetrieverConfig;
import net.meisen.dissertation.models.impl.dataretriever.IQueryConfiguration;

public class RandomDataRetriever extends BaseDataRetriever {

	public RandomDataRetriever(final IDataRetrieverConfig config) {
		super(config);
	}

	@Override
	public DataCollection<?> retrieve(
			final IQueryConfiguration queryConfiguration) {

		return new DataCollection<String>(new String[] { "A" }) {

			@Override
			public DataIterator<String> iterate() {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			public void release() {
				// TODO Auto-generated method stub

			}
		};
	}
}
