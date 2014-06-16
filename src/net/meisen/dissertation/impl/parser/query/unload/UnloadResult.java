package net.meisen.dissertation.impl.parser.query.unload;

import net.meisen.dissertation.model.parser.query.IQueryResultSingleInteger;

/**
 * The result provided by a {@code UnloadQuery}.
 * 
 * @author pmeisen
 * 
 * @see UnloadQuery
 * 
 */
public class UnloadResult implements IQueryResultSingleInteger {

	@Override
	public int getResult() {
		return 0;
	}

	@Override
	public int[] getCollectedIds() {
		return null;
	}
}
