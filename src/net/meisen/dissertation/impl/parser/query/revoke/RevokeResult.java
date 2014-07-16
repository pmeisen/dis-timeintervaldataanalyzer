package net.meisen.dissertation.impl.parser.query.revoke;

import net.meisen.dissertation.model.parser.query.IQueryResultSingleInteger;

/**
 * The result of a {@code RevokeQuery}.
 * 
 * @author pmeisen
 * 
 * @see RevokeQuery
 * 
 */
public class RevokeResult implements IQueryResultSingleInteger {

	@Override
	public int getResult() {
		return 0;
	}

	@Override
	public int[] getCollectedIds() {
		return null;
	}
}
