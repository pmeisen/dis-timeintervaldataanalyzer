package net.meisen.dissertation.impl.parser.query.get;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.jdbc.protocol.QueryType;
import net.meisen.dissertation.model.auth.IAuthManager;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.dissertation.model.auth.permissions.Permission;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IQueryResultSet;
import net.meisen.dissertation.model.parser.query.IResourceResolver;
import net.meisen.dissertation.server.CancellationException;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * A {@code GetQuery} is used to retrieve system-dependent information from the
 * database.
 * 
 * @author pmeisen
 * 
 */
public class GetQuery implements IQuery {
	private GetResultType resultType;

	@Override
	public boolean expectsModel() {
		return false;
	}

	@Override
	public String getModelId() {
		return null;
	}

	@Override
	public void setModelId(final String modelId) {
		// ignore
	}

	@Override
	public IQueryResultSet evaluate(final IAuthManager authManager,
			final TidaModelHandler handler, final TidaModel model,
			final IResourceResolver resolver) throws ForwardedRuntimeException,
			CancellationException {
		if (getResultType().equals(GetResultType.MODELS)) {
			return new GetResultModels(handler.getTidaModels());
		} else if (getResultType().equals(GetResultType.VERSION)) {
			return new GetResultVersion();
		} else if (getResultType().equals(GetResultType.USERS)) {
			return new GetResultUsers(authManager);
		} else if (getResultType().equals(GetResultType.PERMISSIONS)) {
			return new GetResultPermissions(authManager, handler.getTidaModels());
		} else if (getResultType().equals(GetResultType.ROLES)) {
			return new GetResultRoles(authManager);
		}else {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1018, getResultType());
		}
	}

	@Override
	public QueryType getQueryType() {
		return QueryType.QUERY;
	}

	@Override
	public void enableIdCollection(final boolean enableIdCollection) {
		// ignore not supported
	}

	/**
	 * Gets the {@code ResultType} of a {@code this}.
	 * 
	 * @return the {@code ResultType} of a {@code this}
	 */
	public GetResultType getResultType() {
		return resultType;
	}

	/**
	 * Sets the expected {@code GetResultType} of {@code this}.
	 * 
	 * @param resultType
	 *            the expected {@code GetResultType} of {@code this}
	 */
	public void setResultType(final GetResultType resultType) {
		this.resultType = resultType;
	}

	@Override
	public String toString() {
		return "GET " + this.resultType;
	}

	@Override
	public DefinedPermission[][] getNeededPermissions() {
		return new DefinedPermission[][] { new DefinedPermission[] { Permission.get
				.create() } };
	}
}
