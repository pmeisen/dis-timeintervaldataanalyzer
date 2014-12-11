package net.meisen.dissertation.impl.cache;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.io.Serializable;

import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorModelSet;

import org.mapdb.Serializer;

/**
 * {@code Serializer} of a {@code mapDb} used to serialize and deserialize a
 * {@code FactDescriptorModelSet}.
 * 
 * @author pmeisen
 * 
 */
public class MapDbFactDescriptorModelSetSerializer implements
		Serializer<FactDescriptorModelSet>, Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public void serialize(final DataOutput out, final FactDescriptorModelSet fdm)
			throws IOException {
		fdm.serialize(out);
	}

	@Override
	public FactDescriptorModelSet deserialize(final DataInput in,
			final int available) throws IOException {
		return new FactDescriptorModelSet().deserialize(in);
	}

	@Override
	public int fixedSize() {
		return -1;
	}
};
