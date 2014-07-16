package net.meisen.dissertation.impl.parser.query.modify;

import net.meisen.dissertation.model.parser.query.IQueryResultSingleInteger;

/**
 * The result of an {@code ModifyQuery}.
 * 
 * @author pmeisen
 * 
 * @see ModifyQuery
 * 
 */
public class ModifyResult implements IQueryResultSingleInteger {

	@Override
	public int getResult() {
		return 0;
	}

	@Override
	public int[] getCollectedIds() {
		return null;
	}
}
