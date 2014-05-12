package net.meisen.dissertation.model.cache;

import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;

public interface IBitmapOwner {

	public BitmapId<?> getBitmapId();
	
	public void releaseBitmap();
}
