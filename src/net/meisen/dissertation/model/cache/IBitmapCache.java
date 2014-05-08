package net.meisen.dissertation.model.cache;

import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;

public interface IBitmapCache {

	public void setConfig(final IBitmapCacheConfig configuration);
	
	public void registerBitmapUser(final IBitmapUser user);

	public void cacheBitmap(final BitmapId<?> bitmapId, final Bitmap bitmap);

	public Bitmap getBitmap(final BitmapId<?> bitmapId);
}
