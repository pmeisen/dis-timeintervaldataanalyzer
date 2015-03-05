package net.meisen.dissertation.impl.parser.query.select.evaluator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;

public class BaseBitmapPreProcessorObservable implements
		Iterable<IBitmapPreProcessorObserver> {
	private final List<IBitmapPreProcessorObserver> observers;

	public BaseBitmapPreProcessorObservable() {
		this.observers = new ArrayList<IBitmapPreProcessorObserver>();
	}

	public void addObserver(final IBitmapPreProcessorObserver observer) {
		removeObserver(observer);
		this.observers.add(observer);
	}

	public void addObserver(final Iterator<IBitmapPreProcessorObserver> it) {
		while (it.hasNext()) {
			addObserver(it.next());
		}
	}

	public void addObservers(
			final Iterable<IBitmapPreProcessorObserver> observers) {
		addObserver(observers.iterator());
	}

	public void removeObserver(final IBitmapPreProcessorObserver observer) {
		this.observers.remove(observer);
	}

	public boolean hasObserver() {
		return this.observers.size() > 0;
	}

	protected boolean notifyObservers(final String groupId,
			final long normalizedTimePoint, final Bitmap bitmap) {

		for (final IBitmapPreProcessorObserver observer : this) {
			if (!observer.preProcessBitmap(this, groupId, normalizedTimePoint,
					bitmap)) {
				return false;
			}
		}

		return true;
	}

	@Override
	public Iterator<IBitmapPreProcessorObserver> iterator() {
		return this.observers.iterator();
	}
}
