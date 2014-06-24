package net.meisen.dissertation.impl.cache;

import net.meisen.dissertation.model.data.TidaModel;

import org.mapdb.Serializer;

public interface IModelDependendMapDbSerializer<T> extends Serializer<T> {

	public void init(final TidaModel model);

}
