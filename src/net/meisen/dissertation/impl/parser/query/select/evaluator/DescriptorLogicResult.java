package net.meisen.dissertation.impl.parser.query.select.evaluator;

import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;

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
public class DescriptorLogicResult {
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

	/**
	 * Gets the bitmap which determines the selected records.
	 * 
	 * @return bitmap which determines the selected records
	 */
	public Bitmap getBitmap() {
		return bitmap;
	}
}
