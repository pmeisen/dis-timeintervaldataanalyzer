package net.meisen.dissertation.performance.implementations.similarity.tida;

import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

public interface IStructureGroupFactory {

	public StructureGroup create(final int maxPoint) throws ForwardedRuntimeException;
}
