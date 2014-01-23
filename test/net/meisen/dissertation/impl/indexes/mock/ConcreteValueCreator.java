package net.meisen.dissertation.impl.indexes.mock;

/**
 * Helper implementation of a concrete {@code GenericValueCreator}.
 * 
 * @author pmeisen
 * 
 */
public class ConcreteValueCreator extends GenericValueCreator {

	@Override
	public Number getValue1() {
		return 5;
	}

	@Override
	public Object getValue2() {
		return "MyValue";
	}
}
