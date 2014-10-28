package net.meisen.dissertation.impl.indexes;

import net.meisen.dissertation.impl.indexes.datarecord.slices.RoaringBitmap;
import net.meisen.dissertation.model.indexes.IIndexFactoryConfig;
import net.meisen.dissertation.model.indexes.IIndexedCollection;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;

/**
 * Configuration usable to define the indexes created by the
 * {@code IndexFactory}. The configuration can be fed from the xml-configuration
 * as well.
 * 
 * @author pmeisen
 * 
 */
public class IndexFactoryConfig implements IIndexFactoryConfig {
	private Class<? extends IIndexedCollection> byteClass;
	private Class<? extends IIndexedCollection> shortClass;
	private Class<? extends IIndexedCollection> intClass;
	private Class<? extends IIndexedCollection> longClass;

	private Class<? extends Bitmap> bitmapClass;

	/**
	 * Creates a default configuration, which can be modified using the
	 * different getter and setter methods.
	 */
	public IndexFactoryConfig() {

		// by default we use all Trove implementations
		this.byteClass = TroveByteIndexedCollection.class;
		this.shortClass = TroveShortIndexedCollection.class;
		this.intClass = TroveIntIndexedCollection.class;
		this.longClass = TroveLongIndexedCollection.class;
		this.bitmapClass = RoaringBitmap.class;
	}

	/**
	 * Sets the class of the {@code IndexedCollection} to be used for
	 * collections indexed by byte values.
	 * 
	 * @param byteClass
	 *            the class of the {@code IndexedCollection} to be used for
	 *            collections indexed by byte values
	 */
	public void setByteClass(final Class<? extends IIndexedCollection> byteClass) {
		this.byteClass = byteClass;
	}

	/**
	 * Gets the class of the {@code IndexedCollection} used for collections
	 * indexed by byte values.
	 * 
	 * @return the class of the {@code IndexedCollection} used for collections
	 *         indexed by byte values
	 */
	public Class<? extends IIndexedCollection> getByteClass() {
		return byteClass;
	}

	/**
	 * Sets the class of the {@code IndexedCollection} to be used for
	 * collections indexed by short values.
	 * 
	 * @param shortClass
	 *            the class of the {@code IndexedCollection} to be used for
	 *            collections indexed by short values
	 */
	public void setShortClass(
			final Class<? extends IIndexedCollection> shortClass) {
		this.shortClass = shortClass;
	}

	/**
	 * Gets the class of the {@code IndexedCollection} used for collections
	 * indexed by short values.
	 * 
	 * @return the class of the {@code IndexedCollection} used for collections
	 *         indexed by short values
	 */
	public Class<? extends IIndexedCollection> getShortClass() {
		return shortClass;
	}

	/**
	 * Sets the class of the {@code IndexedCollection} to be used for
	 * collections indexed by int values.
	 * 
	 * @param intClass
	 *            the class of the {@code IndexedCollection} to be used for
	 *            collections indexed by int values
	 */
	public void setIntClass(final Class<? extends IIndexedCollection> intClass) {
		this.intClass = intClass;
	}

	/**
	 * Gets the class of the {@code IndexedCollection} used for collections
	 * indexed by int values.
	 * 
	 * @return the class of the {@code IndexedCollection} used for collections
	 *         indexed by int values
	 */
	public Class<? extends IIndexedCollection> getIntClass() {
		return intClass;
	}

	/**
	 * Sets the class of the {@code IndexedCollection} to be used for
	 * collections indexed by long values.
	 * 
	 * @param longClass
	 *            the class of the {@code IndexedCollection} to be used for
	 *            collections indexed by long values
	 */
	public void setLongClass(final Class<? extends IIndexedCollection> longClass) {
		this.longClass = longClass;
	}

	/**
	 * Gets the class of the {@code IndexedCollection} used for collections
	 * indexed by long values.
	 * 
	 * @return the class of the {@code IndexedCollection} used for collections
	 *         indexed by long values
	 */
	public Class<? extends IIndexedCollection> getLongClass() {
		return longClass;
	}

	/**
	 * Get the class used to create a new bitmap instance.
	 * 
	 * @return the class used to create a new bitmap instance
	 * 
	 * @see Bitmap
	 */
	public Class<? extends Bitmap> getBitmapClass() {
		return bitmapClass;
	}

	/**
	 * Defines the class to be used to create a new bitmap instance.
	 * 
	 * @param bitmapClass
	 *            the class to be used to create a new bitmap instance
	 */
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
