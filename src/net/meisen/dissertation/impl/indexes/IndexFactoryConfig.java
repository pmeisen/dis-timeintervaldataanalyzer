package net.meisen.dissertation.impl.indexes;

import net.meisen.dissertation.impl.indexes.datarecord.bitmap.EWAHBitmap;
import net.meisen.dissertation.model.indexes.BaseIndexedCollection;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;

public class IndexFactoryConfig {

	private Class<? extends BaseIndexedCollection> byteClass;
	private Class<? extends BaseIndexedCollection> shortClass;
	private Class<? extends BaseIndexedCollection> intClass;
	private Class<? extends BaseIndexedCollection> longClass;

	private Class<? extends Bitmap> bitmapClass;

	public IndexFactoryConfig() {

		// by default we use all Trove implementations
		this.byteClass = TroveByteIndexedCollection.class;
		this.shortClass = TroveShortIndexedCollection.class;
		this.intClass = TroveIntIndexedCollection.class;
		this.longClass = TroveLongIndexedCollection.class;
		this.bitmapClass = EWAHBitmap.class;
	}

	public void setByteClass(final Class<? extends BaseIndexedCollection> byteClass) {
		this.byteClass = byteClass;
	}

	public Class<? extends BaseIndexedCollection> getByteClass() {
		return byteClass;
	}

	public void setShortClass(
			final Class<? extends BaseIndexedCollection> shortClass) {
		this.shortClass = shortClass;
	}

	public Class<? extends BaseIndexedCollection> getShortClass() {
		return shortClass;
	}

	public void setIntClass(final Class<? extends BaseIndexedCollection> intClass) {
		this.intClass = intClass;
	}

	public Class<? extends BaseIndexedCollection> getIntClass() {
		return intClass;
	}

	public void setLongClass(final Class<? extends BaseIndexedCollection> longClass) {
		this.longClass = longClass;
	}

	public Class<? extends BaseIndexedCollection> getLongClass() {
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
