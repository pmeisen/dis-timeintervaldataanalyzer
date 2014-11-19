package net.meisen.dissertation.impl.parser.query.delete;

import net.meisen.dissertation.model.parser.query.IQueryResultSingleInteger;

public class DeleteResult implements IQueryResultSingleInteger {

	private int amount;
	private int[] ids;

	public DeleteResult() {
		this(0);
	}

	public DeleteResult(final int amount) {
		setAmount(amount);
		this.ids = null;
	}

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

	public int getAmount() {
		return amount;
	}

	public void setAmount(final int amount) {
		this.amount = amount;
	}

	public int[] getIds() {
		return ids;
	}

	public void setIds(final int[] ids) {
		this.ids = ids;
	}

}
