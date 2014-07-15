package net.meisen.dissertation.impl.parser.query.add;

import net.meisen.dissertation.model.parser.query.IQueryResultSingleInteger;

/**
 * The result of an {@code AddQuery}.
 * 
 * @author pmeisen
 * 
 */
public class AddResult implements IQueryResultSingleInteger {

	@Override
	public int getResult() {
		return 0;
	}

	@Override
	public int[] getCollectedIds() {
		return null;
	}
}
