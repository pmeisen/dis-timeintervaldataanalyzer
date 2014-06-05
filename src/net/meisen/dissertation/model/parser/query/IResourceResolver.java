package net.meisen.dissertation.model.parser.query;

import java.io.InputStream;

public interface IResourceResolver {

	public InputStream resolve(final String resource);

}
