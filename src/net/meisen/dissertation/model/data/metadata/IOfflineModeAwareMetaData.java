package net.meisen.dissertation.model.data.metadata;

import net.meisen.dissertation.model.data.OfflineMode;

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
}
