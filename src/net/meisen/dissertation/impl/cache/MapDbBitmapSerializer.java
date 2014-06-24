package net.meisen.dissertation.impl.cache;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.io.Serializable;

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
		IModelDependendMapDbSerializer<Bitmap>, Serializable {
	private static final long serialVersionUID = 1L;

	private transient BaseIndexFactory factory;

	/**
	 * Default constructor, which needs initialization prior to usage.
	 */
	public MapDbBitmapSerializer() {
		this(null);
	}

	/**
	 * Constructor which initializes {@code this}, with the passed (none
	 * {@code null} {@code factory}).
	 * 
	 * @param factory
	 *            the {@code IndexFactory} used to deserialize a {@code Bitmap}
	 * 
	 * @see Bitmap#createFromInput(BaseIndexFactory, DataInput)
	 */
	public MapDbBitmapSerializer(final BaseIndexFactory factory) {
		this.factory = factory;
	}

	@Override
	public void init(final TidaModel model) {
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
