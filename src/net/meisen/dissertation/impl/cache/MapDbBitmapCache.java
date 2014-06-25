package net.meisen.dissertation.impl.cache;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * A cache used to store {@link Bitmap} instances based on a {@code mapDb}.
 * 
 * @author pmeisen
 * 
 */
public class MapDbBitmapCache extends BaseMapDbBitmapIdCache<Bitmap> {

	@Autowired
	@Qualifier(DefaultValues.INDEXFACTORY_ID)
	private BaseIndexFactory factory;

	@Override
	protected String getIndexFileName() {
		return "mapDbBitmap.idx";
	}

	@Override
	protected Bitmap createNewInstance() {
		return factory.createBitmap();
	}

	@Override
	protected MapDbBitmapSerializer createValueSerializer() {
		return new MapDbBitmapSerializer();
	}
}
