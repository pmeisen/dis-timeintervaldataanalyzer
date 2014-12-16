package net.meisen.dissertation.server;

import java.io.File;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import net.meisen.general.server.http.listener.util.RequestFileHandlingUtilities;
import net.meisen.general.server.settings.pojos.Extension;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.http.HttpRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.eclipsesource.json.JsonValue;

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
	protected Object handleRequest(final HttpRequest request,
			final Map<String, String> parameters) throws Exception {

		// parse the request and get the stored file items
		final List<FileItem> fileItems = RequestFileHandlingUtilities
				.handleFileUpload(request, factory);

		// process the uploaded file items
		final Iterator<FileItem> i = fileItems.iterator();
		while (i.hasNext()) {

			final FileItem fi = i.next();
			if (fi.isFormField()) {
				// read field information if needed
			} else if (!fi.isFormField()) {

				// get the submitted client information of the file
				final String fileName = UUID.randomUUID().toString();
				if (LOG.isDebugEnabled()) {
					LOG.debug("Preparing upload of file " + fileName
							+ " (client: " + fi.getName() + ")");
				}

				// define the file
				final File file = new File(sessionManager.getTempDir(),
						fileName);

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

		return JsonValue.NULL;
	}
}
