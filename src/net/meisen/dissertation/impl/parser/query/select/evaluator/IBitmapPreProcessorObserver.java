package net.meisen.dissertation.impl.parser.query.select.evaluator;

import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;

public interface IBitmapPreProcessorObserver {

	public boolean preProcessBitmap(
			final BaseBitmapPreProcessorObservable observable,
			final String groupId, final long normalizedTimePoint,
			final Bitmap bitmap);
}
