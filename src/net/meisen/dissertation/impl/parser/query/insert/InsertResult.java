package net.meisen.dissertation.impl.parser.query.insert;

import net.meisen.dissertation.model.parser.query.IQueryResultSingleInteger;

/**
 * The result of an insert statement.
 * 
 * @author pmeisen
 * 
 */
public class InsertResult implements IQueryResultSingleInteger {

	private int amount;
	private int[] ids;

	/**
	 * Default constructor, initializing the amount with {@code 0}.
	 */
	public InsertResult() {
		this(0);
	}

	/**
	 * Constructor to specify the {@code amount} of data added.
	 * 
	 * @param amount
	 *            the {@code amount} of data added
	 */
	public InsertResult(final int amount) {
		setAmount(amount);

		this.ids = null;
	}

	/**
	 * Constructor to specify the {@code amount} of data added.
	 * 
	 * @param ids
	 *            the {@code ids} of the inserted records
	 */
	public InsertResult(final int[] ids) {
		this(ids.length);

		this.ids = ids;
	}

	/**
	 * Gets the {@code amount} of data added.
	 * 
	 * @return the {@code amount} of data added
	 */
	public int getAmount() {
		return amount;
	}

	/**
	 * Sets the {@code amount} of data added.
	 * 
	 * @param amount
	 *            the {@code amount} of data added
	 */
	public void setAmount(final int amount) {
		this.amount = amount;
	}

	@Override
	public int getResult() {
		return getAmount();
	}

	@Override
	public int[] getCollectedIds() {
		return getIds();
	}

	/**
	 * Depending on the query this method might contain the identifiers added.
	 * 
	 * @return the identifiers added
	 */
	public int[] getIds() {
		return ids;
	}

	/**
	 * Sets the identifiers added by the insertion.
	 * 
	 * @param ids
	 *            the identifiers added
	 */
	public void setIds(final int[] ids) {
		this.ids = ids;
	}
}
