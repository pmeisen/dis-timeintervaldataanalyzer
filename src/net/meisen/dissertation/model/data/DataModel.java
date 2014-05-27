package net.meisen.dissertation.model.data;

import java.util.ArrayList;
import java.util.List;

import net.meisen.dissertation.model.dataretriever.BaseDataRetriever;
import net.meisen.dissertation.model.datasets.IClosableIterator;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.datasets.IDataSet;
import net.meisen.dissertation.model.datasets.MultipleDataSetIterator;
import net.meisen.dissertation.model.indexes.datarecord.IntervalDataHandling;

import org.springframework.beans.factory.annotation.Autowired;

/**
 * The class represents the {@code DataModel}. It can be used to iterate over
 * all the {@code DataSet} instances.
 * 
 * @author pmeisen
 * 
 */
public class DataModel implements IDataSet {

	@Autowired(required = false)
	private List<BaseDataRetriever> retrievers = new ArrayList<BaseDataRetriever>();

	@Autowired(required = false)
	private List<IDataSet> dataSets = new ArrayList<IDataSet>();

	private OfflineMode offlineMode;

	/**
	 * Default constructor
	 */
	public DataModel() {
		setOfflineMode(null);
	}

	@Override
	public IClosableIterator<IDataRecord> iterator() {
		return new MultipleDataSetIterator(getOfflineMode(), dataSets);
	}

	/**
	 * Adds a {@code DataSet} to {@code this}.
	 * 
	 * @param dataSet
	 *            the {@code DataSet} to be added, cannot be {@code null}
	 */
	public void addDataSet(final IDataSet dataSet) {
		if (dataSet == null) {
			return;
		}

		dataSets.add(dataSet);
	}

	@Override
	public boolean hasNamedValue(final String name) {
		if (dataSets == null) {
			return false;
		} else if (dataSets.size() == 0) {
			return false;
		}

		// check if each DataSet supports the name
		for (final IDataSet dataSet : dataSets) {
			if (!dataSet.hasNamedValue(name)) {
				return false;
			}
		}

		return true;
	}

	/**
	 * Checks if there is at least one {@code DataSet} within the
	 * {@code DataModel} which has the specified {@code name}, i.e.
	 * {@link IDataSet#hasNamedValue(String)} returns {@code true}.
	 * 
	 * @param name
	 *            the name to be checked
	 * 
	 * @return {@code true} if there is at least one {@code DataSet} with the
	 *         {@code name}, otherwise {@code false}
	 * 
	 * @see IDataSet
	 */
	public boolean hasNamedValueOnce(final String name) {
		if (dataSets == null) {
			return false;
		} else if (dataSets.size() == 0) {
			return false;
		}

		// check if each DataSet supports the name
		for (final IDataSet dataSet : dataSets) {
			if (dataSet.hasNamedValue(name)) {
				return true;
			}
		}

		return false;
	}

	@Override
	public boolean isValidPosition(final int position) {
		if (dataSets == null) {
			return false;
		} else if (dataSets.size() == 0) {
			return false;
		}

		// check if each DataSet supports the position
		for (final IDataSet dataSet : dataSets) {
			if (!dataSet.isValidPosition(position)) {
				return false;
			}
		}

		return true;
	}

	/**
	 * Checks if there is at least one {@code DataSet} within the
	 * {@code DataModel} with the specified {@code position}, i.e.
	 * {@link IDataSet#isValidPosition(int)} returns {@code true}.
	 * 
	 * @param position
	 *            the position to be checked
	 * 
	 * @return {@code true} if there is at least one {@code DataSet} with the
	 *         {@code position}, otherwise {@code false}
	 * 
	 * @see IDataSet
	 */
	public boolean isValidPositionOnce(final int position) {
		if (dataSets == null) {
			return false;
		} else if (dataSets.size() == 0) {
			return false;
		}

		// check if each DataSet supports the position
		for (final IDataSet dataSet : dataSets) {
			if (dataSet.isValidPosition(position)) {
				return true;
			}
		}

		return false;
	}

	/**
	 * Gets the current setting of the {@code OfflineMode}.
	 * 
	 * @return the settings of the {@code OfflineMode}
	 * 
	 * @see OfflineMode
	 */
	public OfflineMode getOfflineMode() {
		return offlineMode;
	}

	/**
	 * Sets the {@code OfflineMode}, i.e. how invalid data retrievers should be
	 * handled.
	 * 
	 * @param mode
	 *            the {@code OfflineMode} to be used
	 * 
	 * @see IntervalDataHandling
	 */
	public void setOfflineMode(final OfflineMode mode) {
		this.offlineMode = mode == null ? OfflineMode.find(null) : mode;
	}

	/**
	 * Sets the {@code OfflineMode}, i.e. how invalid data retrievers should be
	 * handled.
	 * 
	 * @param mode
	 *            the {@code OfflineMode} to be used
	 * 
	 * @see IntervalDataHandling
	 */
	public void setOfflineModeByString(final String mode) {
		setOfflineMode(OfflineMode.find(mode));
	}

	@Override
	public boolean isOfflineAvailable() {
		for (final IDataSet dataSet : dataSets) {
			if (!dataSet.isOfflineAvailable()) {
				return false;
			}
		}

		return true;
	}

	/**
	 * Gets the {@code BaseDataRetriever} with the specified {@code id}.
	 * 
	 * @param id
	 *            the identifier of the {@code BaseDataRetriever} to retrieve
	 * 
	 * @return the {@code BaseDataRetriever} with the specified {@code id},
	 *         {@code null} if no such {@code BaseDataRetriever} can be found
	 */
	public BaseDataRetriever getDataRetriever(final String id) {
		if (id == null || sizeOfRetrievers() == 0) {
			return null;
		} else {
			for (final BaseDataRetriever retriever : retrievers) {
				if (id.equals(retriever.getId())) {
					return retriever;
				}
			}

			// not found
			return null;
		}
	}

	/**
	 * Gets the amount of defined {@code BaseDataRetriever} instances.
	 * 
	 * @return the amount of defined {@code BaseDataRetriever} instances
	 */
	public int sizeOfRetrievers() {
		return retrievers == null ? 0 : retrievers.size();
	}
}
