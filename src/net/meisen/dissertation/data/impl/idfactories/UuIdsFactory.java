package net.meisen.dissertation.data.impl.idfactories;

import java.util.UUID;

import net.meisen.dissertation.data.IIdsFactory;

/**
 * An implementation of a {@code IdsFactory}, which is based on {@code UUID}.
 * 
 * @see IIdsFactory
 * 
 * @author pmeisen
 * 
 */
public class UuIdsFactory implements IIdsFactory<UUID> {

	@Override
	public Class<UUID> getIdClass() {
		return UUID.class;
	}

	@Override
	public UUID getId() {
		return UUID.randomUUID();
	}
}
