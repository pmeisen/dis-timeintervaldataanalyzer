package net.meisen.dissertation.model.cache;

import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;

/**
 * Marks an instance to be the owner of a {@code Bitmap}. There can only be one
 * owner and the owner has the responsibility to release a {@code Bitmap} if
 * asked for. Releasing means that the pointer to the bitmap is removed so that
 * garbage collection can take place.
 * 
 * @author pmeisen
 * 
 */
public interface IBitmapOwner {

	/**
	 * Gets the identifier of the {@code Bitmap} {@code this} owns.
	 * 
	 * @return the identifier of the {@code Bitmap} {@code this} owns
	 */
	public BitmapId<?> getBitmapId();

	/**
	 * Informs the {@code BitmapOwner} to release the bitmap. Releasing means
	 * that all pointers to the {@code Bitmap} are set to {@code null} so that
	 * garbage collection can take place.
	 */
	public void releaseBitmap();
}
