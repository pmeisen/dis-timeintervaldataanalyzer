package net.meisen.dissertation.data.impl.dataretriever;

public class BaseDataRetriever {
	private IDataRetrieverConfiguration config;

	public BaseDataRetriever(final IDataRetrieverConfiguration config) {
		this.config = config;
	}

	protected IDataRetrieverConfiguration getConfig() {
		return config;
	}
}
