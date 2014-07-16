package net.meisen.dissertation.impl.parser.query.grant;

import net.meisen.dissertation.model.parser.query.IQueryResultSingleInteger;

/**
 * The result of a {@code GrantQuery}.
 * 
 * @author pmeisen
 * 
 * @see GrantQuery
 * 
 */
public class GrantResult implements IQueryResultSingleInteger {

	@Override
	public int getResult() {
		return 0;
	}

	@Override
	public int[] getCollectedIds() {
		return null;
	}
}
