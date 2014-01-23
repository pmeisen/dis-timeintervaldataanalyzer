package net.meisen.dissertation.impl.dataretriever;

import net.meisen.dissertation.model.dataretriever.IQueryConfiguration;

/**
 * A query for a {@code FixedStructureDataCollection}, which defines which data
 * to retrieve.
 * 
 * @author pmeisen
 * 
 */
public class FixedStructureQueryConfig implements IQueryConfiguration {
	private int amount;

	/**
	 * A constructor which defines a query retrieving no data at all.
	 */
	public FixedStructureQueryConfig() {
		this(0);
	}

	/**
	 * A constructor which retrieves the specified {@code amount} of data.
	 * 
	 * @param amount
	 *            the amount of data to be retrieved
	 */
	public FixedStructureQueryConfig(final int amount) {
		this.amount = amount;
	}

	/**
	 * Gets the amount of data to be retrieved.
	 * 
	 * @return the amount of data to be retrieved
	 */
	public int getAmount() {
		return amount;
	}

	/**
	 * Sets the amount of data to be retrieved.
	 * 
	 * @param amount
	 *            the amount of data to be retrieved
	 */
	public void setAmount(int amount) {
		this.amount = amount;
	}

}
