package net.meisen.dissertation.impl.indexes.mock;

import java.util.UUID;

/**
 * A value creator which has to getter methods, which create values.
 * 
 * @author pmeisen
 * 
 */
public abstract class GenericValueCreator {

	/**
	 * First getter...
	 * 
	 * @return some object
	 */
	public Object getValue1() {
		return UUID.randomUUID();
	}

	/**
	 * Second getter...
	 * 
	 * @return some object defined by the concrete implementations
	 */
	public abstract Object getValue2();
}
