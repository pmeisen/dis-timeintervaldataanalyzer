package net.meisen.dissertation.model.cache;

import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;

public interface IBitmapUser {

	public BitmapId<?> getBitmapId();
	
	public void releaseBitmap();
	
	public void updateBitmap(final Bitmap bitmap);
	
	public Bitmap getInstanceBitmap();
}
