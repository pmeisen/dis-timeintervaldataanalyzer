package net.meisen.dissertation.server;

import java.io.UnsupportedEncodingException;

import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.entity.StringEntity;
import org.apache.http.protocol.HttpContext;

import net.meisen.general.server.http.listener.api.IServlet;
import net.meisen.general.server.settings.pojos.Extension;

public class TIDAServerServlet implements IServlet {

	@Override
	public void initialize(final Extension e) {
		// nothing to do
	}

	@Override
	public void handle(final HttpRequest request, final HttpResponse response,
			final HttpContext context) {
		response.setStatusCode(org.apache.http.HttpStatus.SC_OK);

		try {
			StringEntity entity = new StringEntity(
					"THE TEST WAS SUCCESSFUL ON PORT 10001");
			response.setEntity(entity);
		} catch (UnsupportedEncodingException e) {
			response.setStatusCode(org.apache.http.HttpStatus.SC_INTERNAL_SERVER_ERROR);
		}
	}
}
