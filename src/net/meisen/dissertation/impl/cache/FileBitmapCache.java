package net.meisen.dissertation.impl.cache;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.cache.IBitmapIdCache;
import net.meisen.dissertation.model.cache.IReleaseMechanismCache;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Implementation of a {@code BitmapCache}, which uses the file-system to
 * persist bitmaps and loads only a specific amount of bitmaps in memory. <br/>
 * <br/>
 * <b>Note:</b><br/>
 * The implementation is thread-safe.
 * 
 * @author pmeisen
 * 
 */
public class FileBitmapCache extends BaseFileBitmapIdCache<Bitmap> implements
		IBitmapIdCache<Bitmap>, IReleaseMechanismCache<BitmapId<?>, Bitmap> {
	/**
	 * The name of the file used as index-table.
	 */
	protected final static String idxTableFileName = "bitmap.idx";
	/**
	 * The name of the file used to store the bitmaps.
	 */

	@Autowired
	@Qualifier(DefaultValues.INDEXFACTORY_ID)
	private BaseIndexFactory factory;

	@Override
	public Bitmap get(final BitmapId<?> bitmapId) {
		return getCacheable(bitmapId);
	}

	@Override
	protected Bitmap createFromInput(final DataInput in) throws IOException {
		return Bitmap.createFromInput(factory, in);
	}

	@Override
	protected Bitmap createNewInstance() {
		return factory.createBitmap();
	}

	@Override
	protected String getIndexFileName() {
		return idxTableFileName;
	}

	@Override
	protected String getFileName(final int nr) {
		return "bitmap_" + nr + ".data";
	}

	@Override
	protected void writeToOutput(final Bitmap bitmap, final DataOutput out)
			throws IOException {
		bitmap.serialize(out);
	}
}
