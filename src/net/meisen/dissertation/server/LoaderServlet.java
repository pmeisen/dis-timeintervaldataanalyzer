package net.meisen.dissertation.server;

import java.io.UnsupportedEncodingException;

import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.entity.StringEntity;
import org.apache.http.protocol.HttpContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.general.server.http.listener.api.IServlet;
import net.meisen.general.server.settings.pojos.Extension;

public class LoaderServlet implements IServlet {

	@Autowired
	@Qualifier(DefaultValues.MODELHANDLER_ID)
	private TidaModelHandler handler;

	@Override
	public void initialize(final Extension e) {
		// nothing to do
	}

	@Override
	public void handle(final HttpRequest request, final HttpResponse response,
			final HttpContext context) {
		response.setStatusCode(org.apache.http.HttpStatus.SC_OK);
		
		try {
			StringEntity entity = new StringEntity("LOADERSERVLET HERE " + handler);
			response.setEntity(entity);
		} catch (UnsupportedEncodingException e) {
			response.setStatusCode(org.apache.http.HttpStatus.SC_INTERNAL_SERVER_ERROR);
		}
	}
}
