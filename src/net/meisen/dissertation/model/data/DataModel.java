package net.meisen.dissertation.model.data;

import java.util.ArrayList;
import java.util.List;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.datasets.BaseDataSet;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public class DataModel {

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;
	
	@Autowired(required = false)
	private List<BaseDataSet> baseDataSets = new ArrayList<BaseDataSet>();
	
	public void addDataSet(final BaseDataSet dataSet) {
		System.out.println("-------> ADDING dataSet ");
		baseDataSets.add(dataSet);
	}
}
