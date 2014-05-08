package net.meisen.dissertation.impl.parser.query.select.evaluator;

import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;

/**
 * A {@code BitmapResult} is used to mark a result to provide a bitmap, i.e. the
 * result is presented as a bitmap.
 * 
 * @author pmeisen
 * 
 */
public interface IBitmapResult {

	/**
	 * Gets the bitmap of the result.
	 * 
	 * @return the bitmap of the result
	 */
	public Bitmap getBitmap();
}
