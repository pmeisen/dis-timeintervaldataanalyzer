package net.meisen.dissertation.impl.parser.query.delete;

import net.meisen.dissertation.model.parser.query.IQueryResultSingleInteger;

/**
 * The result of a {@code DeleteQuery}.
 * 
 * @author pmeisen
 * 
 */
public class DeleteResult implements IQueryResultSingleInteger {

	private int amount;
	private int[] ids;

	/**
	 * Constructor creating an empty result.
	 */
	public DeleteResult() {
		this(0);
	}

	/**
	 * Constructor creating an result, which specifies the amount of deleted
	 * records.
	 * 
	 * @param amount
	 *            the amount of records deleted
	 */
	public DeleteResult(final int amount) {
		setAmount(amount);
		this.ids = null;
	}

	/**
	 * Constructor creating an result, which specifies the actually deleted
	 * records.
	 * 
	 * @param ids
	 *            the identifiers of the deleted records
	 */
	public DeleteResult(final int[] ids) {
		this(ids.length);
		setIds(ids);
	}

	@Override
	public int[] getCollectedIds() {
		return getIds();
	}

	@Override
	public int getResult() {
		return getAmount();
	}

	/**
	 * Gets the amount of deleted records.
	 * 
	 * @return the amount of deleted records
	 */
	public int getAmount() {
		return amount;
	}

	/**
	 * Specifies the amount of deleted records.
	 * 
	 * @param amount
	 *            the amount of deleted records
	 */
	public void setAmount(final int amount) {
		this.amount = amount;
	}

	/**
	 * Gets the identifiers of records deleted.
	 * 
	 * @return the identifiers of records deleted
	 */
	public int[] getIds() {
		return ids;
	}

	/**
	 * Sets the identifiers of the deleted records.
	 * 
	 * @param ids
	 *            the identifiers of the deleted records
	 */
	public void setIds(final int[] ids) {
		this.ids = ids;
	}

}
