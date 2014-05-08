package net.meisen.dissertation.server;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.impl.parser.query.QueryFactory;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.general.server.api.impl.BaseListener;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public class TidaServerListener extends BaseListener {

	/**
	 * The name under which the listener is registered
	 */
	public static final String NAME = "TSQL";

	private final static Logger LOG = LoggerFactory
			.getLogger(TidaServerListener.class);

	@Autowired
	@Qualifier(DefaultValues.QUERYFACTORY_ID)
	private QueryFactory queryFactory;

	@Override
	protected String handleInput(final String input) {
		
		if (input == null) {
			return null;
		}

		final IQuery query = queryFactory.parseQuery(input);
		queryFactory.evaluateQuery(query);

		return "HELLO";
	}

	@Override
	public String toString() {
		return NAME + (getPort() == -1 ? "" : " (" + getPort() + ")");
	}
}
