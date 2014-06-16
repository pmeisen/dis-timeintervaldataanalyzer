package net.meisen.dissertation.impl.parser.query.alive;

import net.meisen.dissertation.model.parser.query.IQueryResultSingleInteger;

/**
 * The result of an {@code AliveQuery}.
 * 
 * @author pmeisen
 * 
 */
public class AliveResult implements IQueryResultSingleInteger {

	@Override
	public int getResult() {
		return 0;
	}

	@Override
	public int[] getCollectedIds() {
		return null;
	}
}
