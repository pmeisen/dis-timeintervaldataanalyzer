package net.meisen.dissertation.impl.parser.query.assign;

import net.meisen.dissertation.model.parser.query.IQueryResultSingleInteger;

/**
 * The result of an {@code AssignQuery}.
 * 
 * @author pmeisen
 * 
 * @see AssignQuery
 * 
 */
public class AssignResult implements IQueryResultSingleInteger {

	@Override
	public int getResult() {
		return 0;
	}

	@Override
	public int[] getCollectedIds() {
		return null;
	}
}
