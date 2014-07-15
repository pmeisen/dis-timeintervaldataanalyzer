package net.meisen.dissertation.impl.parser.query.alive;

import net.meisen.dissertation.jdbc.protocol.QueryType;
import net.meisen.dissertation.model.auth.IAuthManager;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IQueryResult;
import net.meisen.dissertation.model.parser.query.IResourceResolver;

/**
 * An {@code AliveQuery} is used to see if the connection between client and
 * server is still alive.
 * 
 * @author pmeisen
 * 
 */
public class AliveQuery implements IQuery {

	@Override
	public String getModelId() {
		return null;
	}

	@Override
	public void setModelId(final String modelId) {
		// just ignore it
	}

	@Override
	public IQueryResult evaluate(final IAuthManager authManager,
			final TidaModelHandler handler, final TidaModel model,
			final IResourceResolver resolver) {
		return new AliveResult();
	}

	@Override
	public boolean expectsModel() {
		return false;
	}

	@Override
	public String toString() {
		return "alive";
	}

	@Override
	public QueryType getQueryType() {
		return QueryType.MANIPULATION;
	}

	@Override
	public void enableIdCollection(final boolean enableIdCollection) {
		// ignore not supported
	}

	@Override
	public DefinedPermission[][] getNeededPermissions() {

		// nothing is needed to check alive status
		return new DefinedPermission[][] {};
	}
}
