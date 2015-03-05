package net.meisen.dissertation.performance.implementations.similarity.tida;

import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

public class DefaultStructureGroupFactory implements IStructureGroupFactory {
	private final Class<? extends StructureGroup> clazz;

	public DefaultStructureGroupFactory(Class<? extends StructureGroup> clazz) {
		this.clazz = clazz;
	}

	@Override
	public StructureGroup create(final int maxTimepoint)
			throws ForwardedRuntimeException {

		if (TouchingStructureGroup.class.equals(clazz)) {
			return new TouchingStructureGroup(maxTimepoint);
		} else if (AllStructureGroup.class.equals(clazz)) {
			return new AllStructureGroup(maxTimepoint);
		} else {
			try {
				return clazz.newInstance();
			} catch (final Exception e) {
				// TODO: make nice
				throw new IllegalStateException("", e);
			}
		}
	}
}
