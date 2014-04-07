package net.meisen.dissertation.impl.parser.query.select;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.impl.parser.query.QueryFactory;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.Before;
import org.junit.Test;

@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
public class TestSelectQuery extends ModuleBasedTest {

	private QueryFactory factory;
	private TidaModel model;

	@Before
	public void setModel() {
		factory = new QueryFactory();

		setModulesHolder("/net/meisen/dissertation/impl/parser/query/select/testModel.xml");
		model = modulesHolder.getModule(DefaultValues.TIDAMODEL_ID);

		// load the model
		model.initialize();
		model.loadData();
	}

	/**
	 * Get the factory's result for the {@code query}.
	 * 
	 * @param query
	 *            the query to be created
	 * @return the created {@code Query}
	 */
	@SuppressWarnings("unchecked")
	protected <T extends IQuery> T q(final String query) {
		return (T) factory.parseQuery(query);
	}

	@Test
	public void testExecutionWithSingleFilter() {
		final SelectQuery query = q("select timeseries from testModel in [15.06.2014,20.01.2015] filter by SCREAMS='3'");

		query.execute(model);
	}
	
	@Test
	public void testExecutionWithComplexFilter() {
		final SelectQuery query = q("select timeseries from testModel in [15.06.2014,20.01.2015] filter by (SCREAMS='3' OR SCREAMS='0') AND PERSON='Philipp'");

		query.execute(model);
	}
}
