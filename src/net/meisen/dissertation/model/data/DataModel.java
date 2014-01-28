package net.meisen.dissertation.model.data;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.datasets.DataSetIterator;
import net.meisen.dissertation.model.datasets.IClosableIterator;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.datasets.IDataSet;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public class DataModel {

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	@Autowired(required = false)
	private List<IDataSet> dataSets = new ArrayList<IDataSet>();

	public IClosableIterator<IDataRecord> iterate() {
		return new DataSetIterator(dataSets);
	}

	public void addDataSet(final IDataSet dataSet) {
		dataSets.add(dataSet);
	}
}
