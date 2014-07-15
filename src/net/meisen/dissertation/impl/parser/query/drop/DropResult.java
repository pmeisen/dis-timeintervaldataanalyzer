package net.meisen.dissertation.impl.parser.query.drop;

import net.meisen.dissertation.model.parser.query.IQueryResultSingleInteger;

/**
 * The result of an {@code DropQuery}.
 * 
 * @author pmeisen
 * 
 */
public class DropResult implements IQueryResultSingleInteger {

	@Override
	public int getResult() {
		return 0;
	}

	@Override
	public int[] getCollectedIds() {
		return null;
	}
}
