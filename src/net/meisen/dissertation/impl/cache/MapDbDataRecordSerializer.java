package net.meisen.dissertation.impl.cache;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;

import net.meisen.dissertation.jdbc.protocol.DataType;
import net.meisen.dissertation.model.data.TidaModel;

/**
 * Serializer used to serialize the values of a record. The serialization is
 * mainly based on {@code DataType#write(DataOutput, Object)} and
 * deserialization is based on {@code DataType#read(DataInput)}.
 * 
 * @author pmeisen
 * 
 */
public class MapDbDataRecordSerializer implements
		IModelDependendMapDbSerializer<Object[]> {
	private static final long serialVersionUID = 1L;

	private transient DataType[] types;

	@Override
	public void init(final BaseMapDbCache<?, ?> cache, final TidaModel model) {
		types = ((MapDbDataRecordCache) cache).getDataTypes();
	}

	@Override
	public int fixedSize() {
		return -1;
	}

	@Override
	public void serialize(final DataOutput out, final Object[] value)
			throws IOException {

		for (int i = 0; i < types.length; i++) {
			final DataType type = types[i];
			type.write(out, value[i]);
		}
	}

	@Override
	public Object[] deserialize(final DataInput in, final int available)
			throws IOException {
		final Object[] values = new Object[types.length];

		for (int i = 0; i < types.length; i++) {
			final DataType type = types[i];
			values[i] = type.read(in);
		}

		return values;
	}
}
