package net.meisen.dissertation.impl.indexes;

import net.meisen.dissertation.model.indexes.IndexedCollection;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.EWAHBitmap;

public class IndexedCollectionFactoryConfig {

	private Class<? extends IndexedCollection> byteClass;
	private Class<? extends IndexedCollection> shortClass;
	private Class<? extends IndexedCollection> intClass;
	private Class<? extends IndexedCollection> longClass;

	private Class<? extends Bitmap> bitmapClass;

	public IndexedCollectionFactoryConfig() {

		// by default we use all Trove implementations
		this.byteClass = TroveByteIndexedCollection.class;
		this.shortClass = TroveShortIndexedCollection.class;
		this.intClass = TroveIntIndexedCollection.class;
		this.longClass = TroveLongIndexedCollection.class;
		this.bitmapClass = EWAHBitmap.class;
	}

	public void setByteClass(final Class<? extends IndexedCollection> byteClass) {
		this.byteClass = byteClass;
	}

	public Class<? extends IndexedCollection> getByteClass() {
		return byteClass;
	}

	public void setShortClass(
			final Class<? extends IndexedCollection> shortClass) {
		this.shortClass = shortClass;
	}

	public Class<? extends IndexedCollection> getShortClass() {
		return shortClass;
	}

	public void setIntClass(final Class<? extends IndexedCollection> intClass) {
		this.intClass = intClass;
	}

	public Class<? extends IndexedCollection> getIntClass() {
		return intClass;
	}

	public void setLongClass(final Class<? extends IndexedCollection> longClass) {
		this.longClass = longClass;
	}

	public Class<? extends IndexedCollection> getLongClass() {
		return longClass;
	}

	public Class<? extends Bitmap> getBitmapClass() {
		return bitmapClass;
	}

	public void setBitmapClass(final Class<? extends Bitmap> bitmapClass) {
		this.bitmapClass = bitmapClass;
	}

	public String toString() {
		return "byteClass: " + byteClass.getName() + ", shortClass: "
				+ shortClass.getName() + ", intClass: " + intClass.getName()
				+ ", longClass: " + longClass.getName() + ", bitmapClass: "
				+ bitmapClass.getName();
	}
}
