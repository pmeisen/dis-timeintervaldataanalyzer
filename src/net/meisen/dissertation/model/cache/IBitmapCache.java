package net.meisen.dissertation.model.cache;

import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;

public interface IBitmapCache {

	public void initialize(final TidaModel model);

	public void setConfig(final IBitmapCacheConfig configuration);

	public void registerBitmapOwner(final IBitmapOwner user);

	public void cacheBitmap(final BitmapId<?> bitmapId, final Bitmap bitmap);

	public Bitmap getBitmap(final BitmapId<?> bitmapId);

	public void release();
}
