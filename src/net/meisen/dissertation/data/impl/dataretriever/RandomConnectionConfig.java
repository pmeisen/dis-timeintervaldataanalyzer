package net.meisen.dissertation.data.impl.dataretriever;

import net.meisen.dissertation.models.impl.dataretriever.IDataRetrieverConfiguration;

public class RandomConnectionConfig implements IDataRetrieverConfiguration {
	private int amount;
	private Class<?> type;

	public int getAmount() {
		return amount;
	}

	public void setAmount(final int amount) {
		this.amount = amount;
	}

	public Class<?> getType() {
		return type;
	}

	public void setType(final Class<?> type) {
		this.type = type;
	}
}
