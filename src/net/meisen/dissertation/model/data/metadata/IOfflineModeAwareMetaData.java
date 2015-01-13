package net.meisen.dissertation.model.data.metadata;

import net.meisen.dissertation.model.data.OfflineMode;
import net.meisen.dissertation.model.indexes.datarecord.IntervalDataHandling;

/**
 * A offline-aware {@code MetaData} is a {@code MetaData} instance which
 * receives the values of the descriptor from an external source. Therefore the
 * definition of the {@code OfflineMode} is important to those instances.
 * 
 * @author pmeisen
 * 
 * @see OfflineMode
 * 
 */
public interface IOfflineModeAwareMetaData extends IMetaData {

	/**
	 * Sets the {@code OfflineMode} to be used by the {@code MetaData}.
	 * 
	 * @param offlineMode
	 *            the {@code OfflineMode} to be used
	 * 
	 * @see OfflineMode
	 */
	public void setOfflineMode(final OfflineMode offlineMode);

	/**
	 * Sets the {@code OfflineMode}, i.e. how invalid data retrievers should be
	 * handled.
	 * 
	 * @param mode
	 *            the {@code OfflineMode} to be used
	 * 
	 * @see IntervalDataHandling
	 */
	public void setOfflineModeByString(String mode);
}
