package net.meisen.dissertation.impl.parser.query.remove;

import net.meisen.dissertation.model.parser.query.IQueryResultSingleInteger;

/**
 * The result of an {@code RemoveQuery}.
 * 
 * @author pmeisen
 * 
 * @see RemoveQuery
 * 
 */
public class RemoveResult implements IQueryResultSingleInteger {

	@Override
	public int getResult() {
		return 0;
	}

	@Override
	public int[] getCollectedIds() {
		return null;
	}
}
