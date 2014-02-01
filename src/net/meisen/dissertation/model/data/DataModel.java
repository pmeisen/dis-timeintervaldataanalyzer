package net.meisen.dissertation.model.data;

import java.util.ArrayList;
import java.util.List;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.datasets.MultipleDataSetIterator;
import net.meisen.dissertation.model.datasets.IClosableIterator;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.datasets.IDataSet;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public class DataModel implements IDataSet {

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	@Autowired(required = false)
	private List<IDataSet> dataSets = new ArrayList<IDataSet>();

	@Override
	public IClosableIterator<IDataRecord> iterate() {
		return new MultipleDataSetIterator(dataSets);
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
	public boolean isValidPosition(int position) {
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
	public boolean isValidPositionOnce(int position) {
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
}
