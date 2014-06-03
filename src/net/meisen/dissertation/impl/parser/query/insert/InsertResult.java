package net.meisen.dissertation.impl.parser.query.insert;

import net.meisen.dissertation.model.parser.query.IQueryResult;

/**
 * The result of an insert statement.
 * 
 * @author pmeisen
 * 
 */
public class InsertResult implements IQueryResult {

	private int amount;

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
}
