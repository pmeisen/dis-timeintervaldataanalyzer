package net.meisen.dissertation.data.impl.indexes.mock;

/**
 * The implementation overrides the getter methods of the
 * {@code ConcreteValueCreator}.
 * 
 * @author pmeisen
 * 
 */
public class OverrideValueCreator extends ConcreteValueCreator {

	@Override
	public String getValue2() {
		return "IDONTHAVEAVALUE";
	}
}
