package net.meisen.dissertation.impl.parser.query.select.evaluator;

import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;

/**
 * A {@code DescriptorLogicResult} is the result of the application of a
 * {@code DescriptorLogicTree} to a specific {@code TidaModel}. The result
 * contains a bitmap addressing the selected records of the model as defined by
 * the {@code DescriptorLogicTree}.
 * 
 * @author pmeisen
 * 
 * @see DescriptorLogicEvaluator
 * 
 */
public class DescriptorLogicResult implements IBitmapResult {
	private final Bitmap bitmap;

	/**
	 * Constructor used to create the result.
	 * 
	 * @param bitmap
	 *            the bitmap which determines the selected records
	 */
	public DescriptorLogicResult(final Bitmap bitmap) {
		this.bitmap = bitmap;
	}

	@Override
	public Bitmap getBitmap() {
		return bitmap;
	}

	@Override
	public String toString() {
		return bitmap == null ? null : bitmap.toString();
	}
}
