package net.meisen.dissertation.impl.data.metadata;

import java.util.ArrayList;
import java.util.Collection;

import net.meisen.dissertation.model.data.OfflineMode;
import net.meisen.dissertation.model.data.metadata.IOfflineModeAwareMetaData;
import net.meisen.dissertation.model.dataretriever.BaseDataRetriever;
import net.meisen.dissertation.model.dataretriever.DataCollection;
import net.meisen.dissertation.model.dataretriever.IQueryConfiguration;
import net.meisen.general.genmisc.types.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A {@code DbMetaData} is used to retrieve meta-data from a
 * {@code BaseDataRetriever}.
 * 
 * @author pmeisen
 * 
 */
public class DbMetaData implements IOfflineModeAwareMetaData {
	private final static Logger LOG = LoggerFactory.getLogger(DbMetaData.class);

	private final BaseDataRetriever retriever;
	private final IQueryConfiguration query;
	private final String descriptorModelId;

	private OfflineMode offlineMode;

	/**
	 * Constructor to specify the {@code descriptorModelId}, the
	 * {@code retriever}, and the {@code query}.
	 * 
	 * @param descriptorModelId
	 *            the identifier of the {@code DescriptorModel} the values
	 *            belong to
	 * @param retriever
	 *            the {@code BaseDataRetriever} to retrieve the data from
	 * @param query
	 *            the query to be fired
	 */
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
	public void setOfflineModeByString(final String mode) {
		setOfflineMode(OfflineMode.find(mode));
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

	@Override
	public int size() {
		return getValues().size();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == this) {
			return true;
		} else if (obj instanceof DbMetaData) {
			final DbMetaData dbmd = (DbMetaData) obj;
			return Objects.equals(dbmd.descriptorModelId, descriptorModelId)
					&& Objects.equals(dbmd.retriever, retriever)
					&& Objects.equals(dbmd.query, query);
		} else {
			return false;
		}
	}
}
