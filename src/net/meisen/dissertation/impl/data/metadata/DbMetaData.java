package net.meisen.dissertation.impl.data.metadata;

import java.util.ArrayList;
import java.util.Collection;

import net.meisen.dissertation.model.data.OfflineMode;
import net.meisen.dissertation.model.data.metadata.IMetaData;
import net.meisen.dissertation.model.data.metadata.IOfflineModeAwareMetaData;
import net.meisen.dissertation.model.dataretriever.BaseDataRetriever;
import net.meisen.dissertation.model.dataretriever.DataCollection;
import net.meisen.dissertation.model.dataretriever.IQueryConfiguration;
import net.meisen.dissertation.model.indexes.datarecord.IntervalDataHandling;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DbMetaData implements IOfflineModeAwareMetaData {
	private final static Logger LOG = LoggerFactory.getLogger(DbMetaData.class);

	private final BaseDataRetriever retriever;
	private final IQueryConfiguration query;
	private final String descriptorModelId;

	private OfflineMode offlineMode;

	public DbMetaData(final String descriptorModelId,
			final BaseDataRetriever retriever, final IQueryConfiguration query) {
		this.descriptorModelId = descriptorModelId;

		this.retriever = retriever;
		this.query = query;

		setOfflineMode(null);
	}
	
	/**
	 * Gets the {@code OfflineMode} defined for the {@code DescriptorModel}.
	 * 
	 * @return the {@code OfflineMode}
	 * 
	 * @see OfflineMode
	 */
	public OfflineMode getOfflineMode() {
		return offlineMode;
	}

	@Override
	public void setOfflineMode(final OfflineMode offlineMode) {
		this.offlineMode = offlineMode == null ? OfflineMode.find(null)
				: offlineMode;
	}

	@Override
	public Collection<Object> getValues() {
		Collection<Object> loadedValues;

		// check if we are in offline mode
		if (OfflineMode.TRUE.equals(getOfflineMode())) {
			loadedValues = new ArrayList<Object>();
		} else {
			DataCollection<?> loader = null;

			try {
				loader = retriever.retrieve(query);
				loadedValues = loader.transform();
			} catch (final RuntimeException e) {
				if (OfflineMode.FALSE.equals(getOfflineMode())) {
					throw e;
				} else {
					if (LOG.isTraceEnabled()) {
						LOG.trace("Could not load the values (OfflineMode: "
								+ getOfflineMode() + ") defined by '" + query
								+ "'.");
					}

					loadedValues = new ArrayList<Object>();
				}
			} finally {
				if (loader != null) {
					try {
						loader.release();
					} catch (final RuntimeException e) {
						// ignore
					}
				}
			}
		}

		return loadedValues;
	}

	@Override
	public String getDescriptorModelId() {
		return descriptorModelId;
	}

	@Override
	public String toString() {
		return getDescriptorModelId() + ": " + retriever + " - " + query;
	}
}
