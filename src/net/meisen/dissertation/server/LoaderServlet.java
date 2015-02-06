package net.meisen.dissertation.server;

import java.io.File;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import net.meisen.dissertation.server.sessions.Session;
import net.meisen.general.server.http.listener.util.RequestFileHandlingUtilities;
import net.meisen.general.server.settings.pojos.Extension;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.http.HttpRequest;
import org.apache.http.entity.ContentType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.eclipsesource.json.JsonArray;
import com.eclipsesource.json.JsonObject;

/**
 * A servlet used to load files within the current session.
 * 
 * @author pmeisen
 * 
 */
public class LoaderServlet extends BaseServlet {
	private final static Logger LOG = LoggerFactory
			.getLogger(LoaderServlet.class);

	/**
	 * Convenient factory to handle the file upload
	 */
	private DiskFileItemFactory factory;

	@Override
	public void initialize(final Extension e) {
		factory = new DiskFileItemFactory();
		factory.setRepository(sessionManager.getTempDir());
	}

	@Override
	protected boolean needValidSession() {
		return false;
	}

	@Override
	protected boolean doHttpPermissionCheck() {
		return false;
	}

	/**
	 * Gets the parameters and validates the authentication, reading the data
	 * from the specified iterator.
	 * 
	 * @param it
	 *            the iterator containing all the parameters specified
	 * 
	 * @return the read parameters
	 */
	protected Map<String, String> getParametersAndValidateAuth(
			final Iterator<FileItem> it) {
		final Map<String, String> parameters = new HashMap<String, String>();

		while (it.hasNext()) {
			final FileItem fi = it.next();
			if (fi.isFormField()) {

				// read field information if needed
				if (LOG.isTraceEnabled()) {
					LOG.trace("Got formField '" + fi.getFieldName()
							+ "' with value '" + fi.getString());
				}
				parameters.put(fi.getFieldName(), fi.getString());

				// check if we got an id
				if (BaseServlet.PARAM_SESSIONID.equals(fi.getFieldName())) {

					// check the user in
					final Session session = checkSession(fi.getString());
					session.markAsUsed();
				}
			}
		}

		// do a check first
		checkHttpPermission();

		return parameters;
	}

	@Override
	protected HandleResult _handle(final HttpRequest request) throws Exception {

		// parse the request and get the stored file items
		final List<FileItem> fileItems = RequestFileHandlingUtilities
				.handleFileUpload(request, factory);

		// validate everything
		final Map<String, String> params = getParametersAndValidateAuth(fileItems
				.iterator());

		// process the uploaded file items
		final Iterator<FileItem> i = fileItems.iterator();
		final JsonArray jsonFiles = new JsonArray();
		while (i.hasNext()) {

			final FileItem fi = i.next();
			if (!fi.isFormField()) {

				// get the submitted client information of the file
				final String fileName = UUID.randomUUID().toString();
				if (LOG.isDebugEnabled()) {
					LOG.debug("Preparing upload of file " + fileName
							+ " (client: " + fi.getName() + ")");
				}

				// define the file
				final File file = new File(sessionManager.getSessionDir(
						params.get(BaseServlet.PARAM_SESSIONID), true),
						fileName);
				final JsonObject jsonFile = new JsonObject();
				jsonFile.add("fileName", fileName);
				jsonFile.add("fromField", fi.getFieldName());
				jsonFile.add("orgFile", fi.getName());
				jsonFiles.add(jsonFile);

				// write the file
				try {
					fi.write(file);
				} catch (final Exception e) {
					if (!file.delete()) {
						file.deleteOnExit();
					}
				}
			}
		}

		return new HandleResult(jsonFiles.toString(),
				ContentType.APPLICATION_JSON);
	}

	@Override
	protected Object handleRequest(final HttpRequest request,
			final Map<String, String> parameters) throws Exception {
		// this method is never called
		return null;
	}
}
