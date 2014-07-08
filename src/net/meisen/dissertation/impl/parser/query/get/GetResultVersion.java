package net.meisen.dissertation.impl.parser.query.get;

import java.io.IOException;
import java.util.Collections;
import java.util.Iterator;

import net.meisen.dissertation.model.data.FieldNameGenerator;
import net.meisen.dissertation.model.parser.query.IQueryResultSet;
import net.meisen.dissertation.server.TidaServer;
import net.meisen.general.genmisc.resources.Manifest;
import net.meisen.general.genmisc.resources.ManifestInfo;

/**
 * Gets a query asking for a {@link GetResultType#VERSION}.
 * 
 * @author pmeisen
 * 
 */
public class GetResultVersion implements IQueryResultSet {

	private final ManifestInfo manifest;

	/**
	 * Default constructor.
	 */
	public GetResultVersion() {
		ManifestInfo manifest;
		try {
			manifest = Manifest.getManifestInfo(TidaServer.class);
		} catch (final IOException e) {
			manifest = null;
		}

		this.manifest = manifest;
	}

	/**
	 * Checks if a {@code ManifestInfo} is available.
	 * 
	 * @return {@code true} if one is available, otherwise {@code false}
	 */
	public boolean hasManifest() {
		return manifest != null;
	}

	@Override
	public int[] getCollectedIds() {
		return null;
	}

	@Override
	public Iterator<Object[]> iterator() {

		if (manifest == null) {
			return Collections.<Object[]> emptyList().iterator();
		} else {
			return new Iterator<Object[]>() {
				private boolean hasNext = true;

				@Override
				public boolean hasNext() {
					return hasNext;
				}

				@Override
				public Object[] next() {
					if (!hasNext()) {
						throw new IllegalStateException(
								"No more data available.");
					}
					this.hasNext = false;

					return new Object[] { manifest.getImplementationTitle(),
							manifest.getImplementationVersion(),
							manifest.getSVNRevision() };
				}

				@Override
				public void remove() {
					throw new UnsupportedOperationException(
							"Remove is not supported.");
				}
			};
		}
	}

	@Override
	public Class<?>[] getTypes() {
		return new Class<?>[] { String.class, String.class, String.class };
	}

	@Override
	public String[] getNames() {
		final FieldNameGenerator fng = FieldNameGenerator.get();
		return new String[] { fng.getDbTitleFieldName(),
				fng.getDbVersionFieldName(), fng.getInternalRevisionFieldName() };
	}

}
