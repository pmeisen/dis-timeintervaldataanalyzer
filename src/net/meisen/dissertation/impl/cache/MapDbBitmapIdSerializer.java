package net.meisen.dissertation.impl.cache;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.io.Serializable;

import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;

import org.mapdb.Serializer;

/**
 * A {@code Serializer} used to serialize and deserialize a {@code BitmapId}.
 * 
 * @author pmeisen
 * 
 */
public class MapDbBitmapIdSerializer implements Serializer<BitmapId<?>>,
		Serializable {
	private static final long serialVersionUID = 1l;

	@Override
	public void serialize(final DataOutput out, final BitmapId<?> value)
			throws IOException {

		final byte[] bytes = value.bytes();
		final byte[] paddedBytes;
		if (bytes.length == BitmapId.getMaxBytesLength()) {
			paddedBytes = bytes;
		} else {
			paddedBytes = new byte[BitmapId.getMaxBytesLength()];
			System.arraycopy(bytes, 0, paddedBytes, 0, bytes.length);
		}

		out.write(paddedBytes);
	}

	@Override
	public BitmapId<?> deserialize(final DataInput in, final int available)
			throws IOException {
		final byte[] bytes = new byte[BitmapId.getMaxBytesLength()];
		in.readFully(bytes);

		@SuppressWarnings("rawtypes")
		final BitmapId<?> id = new BitmapId(bytes);

		return id;
	}

	@Override
	public int fixedSize() {
		return BitmapId.getMaxBytesLength();
	}
}
