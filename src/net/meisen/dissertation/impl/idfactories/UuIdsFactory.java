package net.meisen.dissertation.impl.idfactories;

import java.util.UUID;

import net.meisen.dissertation.model.idfactories.IIdsFactory;

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

	@Override
	public void setIdAsUsed(final UUID id) {
		// nothing to do we don't expect anyone to guess a UUID
	}
}
