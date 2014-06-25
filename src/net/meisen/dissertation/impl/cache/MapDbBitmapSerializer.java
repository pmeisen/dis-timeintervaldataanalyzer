package net.meisen.dissertation.impl.cache;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;

import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;

/**
 * {@code Serializer} of a {@code mapDb} used to serialize and deserialize a
 * {@code Bitmap}.
 * 
 * @author pmeisen
 * 
 */
public class MapDbBitmapSerializer implements
		IModelDependendMapDbSerializer<Bitmap> {
	private static final long serialVersionUID = 1L;

	private transient BaseIndexFactory factory;

	@Override
	public void init(final BaseMapDbCache<?, ?> cache, final TidaModel model) {
		factory = model.getIndexFactory();
	}

	@Override
	public void serialize(final DataOutput out, final Bitmap bitmap)
			throws IOException {
		bitmap.serialize(out);
	}

	@Override
	public Bitmap deserialize(final DataInput in, final int available)
			throws IOException {
		return Bitmap.createFromInput(factory, in);
	}

	@Override
	public int fixedSize() {
		return -1;
	}
}
