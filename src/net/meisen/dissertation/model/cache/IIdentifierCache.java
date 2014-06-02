package net.meisen.dissertation.model.cache;

import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;

public interface IIdentifierCache {

	public void initialize(final TidaModel model);

	public int getLastUsedIdentifier();

	public Bitmap getValidIdentifiers();

	public void markIdentifierAsUsed(final int lastUsedIdentifier);

	/**
	 * Marks the identifiers as valid, i.e. to be used.
	 * 
	 * @param validIdentifier
	 *            the identifiers to be marked as used
	 */
	public void markIdentifierAsValid(final int... validIdentifier);

	/**
	 * Marks the identifiers as invalid, i.e. not to be used.
	 * 
	 * @param invalidIdentifier
	 *            the identifiers marked as not to be used
	 */
	public void markIdentifierAsInvalid(final int... invalidIdentifier);

	public boolean setPersistency(final boolean enable);

	public void release();

	void setConfig(final IIdentifierCacheConfig config);
}
