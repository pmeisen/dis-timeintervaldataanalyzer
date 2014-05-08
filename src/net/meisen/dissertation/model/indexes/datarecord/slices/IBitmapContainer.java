package net.meisen.dissertation.model.indexes.datarecord.slices;

/**
 * A {@code BitmapContainer} is used to mark an instance to provide a bitmap,
 * i.e. a bitmap is contained within the instance.
 * 
 * @author pmeisen
 * 
 */
public interface IBitmapContainer {

	/**
	 * Gets the contained bitmap.
	 * 
	 * @return the contained bitmap
	 */
	public Bitmap getBitmap();
}
