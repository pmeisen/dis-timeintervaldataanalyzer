package net.meisen.dissertation.server;

import java.io.UnsupportedEncodingException;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.impl.parser.query.QueryFactory;
import net.meisen.general.server.http.listener.api.IServlet;
import net.meisen.general.server.settings.pojos.Extension;

import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.entity.StringEntity;
import org.apache.http.protocol.HttpContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public class QueryServlet implements IServlet {

	@Autowired
	@Qualifier(DefaultValues.QUERYFACTORY_ID)
	private QueryFactory queryFactory;

	@Override
	public void initialize(final Extension e) {
		// nothing to do
	}

	@Override
	public void handle(final HttpRequest request, final HttpResponse response,
			final HttpContext context) {
		response.setStatusCode(org.apache.http.HttpStatus.SC_OK);
		
//		final String queryString = "select";
//		final IQuery query = queryFactory.parseQuery(queryString);
//		queryFactory.evaluateQuery(query);

		try {
			StringEntity entity = new StringEntity("QUERYSERVLET HERE " + queryFactory );
			response.setEntity(entity);
		} catch (UnsupportedEncodingException e) {
			response.setStatusCode(org.apache.http.HttpStatus.SC_INTERNAL_SERVER_ERROR);
		}
	}
}
