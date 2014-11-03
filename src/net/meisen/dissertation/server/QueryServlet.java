package net.meisen.dissertation.server;

import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.PermissionException;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.dissertation.model.auth.permissions.Permission;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IQueryFactory;
import net.meisen.dissertation.model.parser.query.IQueryResult;
import net.meisen.dissertation.model.parser.query.IQueryResultSet;
import net.meisen.dissertation.model.parser.query.IQueryResultSingleInteger;
import net.meisen.general.genmisc.types.Dates;
import net.meisen.general.server.http.listener.util.RequestHandlingUtilities;
import net.meisen.general.server.settings.pojos.Extension;

import org.apache.http.HttpRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import com.eclipsesource.json.JsonArray;
import com.eclipsesource.json.JsonObject;
import com.eclipsesource.json.JsonValue;
import com.eclipsesource.json.ParseException;

public class QueryServlet extends BaseServlet {

	@Autowired
	@Qualifier(DefaultValues.QUERYFACTORY_ID)
	private IQueryFactory queryFactory;

	@Override
	public void initialize(final Extension e) {
		// nothing to do
	}

	@Override
	protected Object handleRequest(final HttpRequest request,
			final Map<String, String> parameters) throws Exception {
		final String path = RequestHandlingUtilities.getPath(request);
		final int pos = path.lastIndexOf("/");
		final String method = pos == -1 ? "" : path.substring(pos + 1);

		// check the failure handling
		if ("tsql".equals(method)) {
			return execQuery(parameters);
		} else if ("system".equals(method)) {
			return execSystemQuery(parameters);
		} else {
			// TODO throw exception
			throw new IllegalStateException("Unsupported method '" + method
					+ "' called.");
		}
	}

	protected Object execSystemQuery(final Map<String, String> parameters)
			throws Exception {

		String o;
		if ((o = parameters.get("object")) != null) {
			if ("permissions".equals(o)) {

				// check the permission to use this system retrieval
				if (!authManager.hasPermission(Permission.manageUsers.create())) {
					exceptionRegistry.throwRuntimeException(
							PermissionException.class, 1000,
							Permission.manageUsers);
				}

				// get the permissions
				final JsonArray array = new JsonArray();
				for (final String p : Permission.transform(Permission.values(),
						"*", DefinedPermission.DEF_SEPARATOR)) {
					array.add(p);
				}

				// return the permissions
				return array.toString();
			} else if ("user".equals(o)) {

				// check the permission to use this system retrieval
				if (!authManager.hasPermission(Permission.manageUsers.create())) {
					exceptionRegistry.throwRuntimeException(
							PermissionException.class, 1000,
							Permission.manageUsers);
				}

				final String username = parameters.get("username");
				final String includeRoleString = parameters.get("includeRole");
				final boolean includeRole = includeRoleString != null
						&& includeRoleString.equals("true");

				// get the permissions
				final Set<DefinedPermission> perms = includeRole ? authManager
						.getUserPermissions(username) : authManager
						.getAssignedUserPermissions(username);
				final JsonArray permArray = new JsonArray();
				for (final DefinedPermission p : perms) {
					permArray.add(p.toString(DefinedPermission.DEF_SEPARATOR));
				}

				// get the roles
				final Set<String> roles = authManager.getUserRoles(username);
				final JsonArray rolesArray = new JsonArray();
				for (final String r : roles) {
					rolesArray.add(r);
				}

				// create the response
				final JsonObject object = new JsonObject();
				object.add("permissions", permArray);
				object.add("roles", rolesArray);
				object.add("username", JsonValue.valueOf(username));

				return object.toString();
			} else if ("role".equals(o)) {

				// check the permission to use this system retrieval
				if (!authManager.hasPermission(Permission.manageUsers.create())) {
					exceptionRegistry.throwRuntimeException(
							PermissionException.class, 1000,
							Permission.manageUsers);
				}

				final String rolename = parameters.get("rolename");

				// get the permissions
				final Set<DefinedPermission> perms = authManager
						.getRolePermissions(rolename);
				final JsonArray permArray = new JsonArray();
				for (final DefinedPermission p : perms) {
					permArray.add(p.toString(DefinedPermission.DEF_SEPARATOR));
				}

				// create the response
				final JsonObject object = new JsonObject();
				object.add("permissions", permArray);
				object.add("rolename", JsonValue.valueOf(rolename));

				return object.toString();
			} else {
				throw new IllegalStateException("Unsupported object '" + o
						+ "' defined.");
			}
		} else {
			// TODO throw exception
			throw new IllegalStateException("Object is not defined.");
		}
	}

	protected Object execQuery(final Map<String, String> parameters)
			throws Exception {
		final String failure = parameters.get("failOnFailure");
		final boolean failOnFailure = failure == null
				|| "true".equalsIgnoreCase(failure);

		// check if we have a single query or several queries
		String q;
		JsonValue value;
		boolean singleQuery = true;
		if ((q = parameters.get("query")) != null) {
			try {
				value = JsonValue.readFrom(q);
			} catch (final ParseException e) {
				// try to use the query as string
				value = JsonValue.valueOf(q);
			}
		} else if ((q = parameters.get("queries")) != null) {
			try {
				value = JsonValue.readFrom(q);
			} catch (final ParseException e) {
				// try to use the query as string
				value = JsonValue.valueOf(q);
			}
			singleQuery = false;
		} else {
			value = null;
		}

		// get the defined queries
		final List<String> queries = new ArrayList<String>();
		if (value == null) {
			// do nothing
		} else if (value.isString()) {
			queries.add(value.asString());
		} else if (value.isArray()) {
			for (final JsonValue v : value.asArray().values()) {
				queries.add(v.asString());
			}
		}

		// fire the query
		final JsonArray results = new JsonArray();
		for (final String query : queries) {
			IQueryResult res;
			try {
				final IQuery parsedQuery = queryFactory.parseQuery(query);
				// TODO we have to add the resolver
				res = queryFactory.evaluateQuery(parsedQuery, null);
			} catch (final Exception e) {
				if (failOnFailure) {
					throw e;
				} else {
					res = null;
				}
			}

			// check if we have a res
			final JsonValue resultValue;
			final JsonValue resultType;
			if (res instanceof IQueryResultSet) {

				// iterate over the records and add those
				final IQueryResultSet resSet = (IQueryResultSet) res;
				final JsonArray array = new JsonArray();
				final Iterator<Object[]> it = resSet.iterator();
				while (it.hasNext()) {
					final Object[] objs = it.next();
					final JsonArray row = new JsonArray();
					for (final Object obj : objs) {
						final JsonValue objValue;

						if (obj == null) {
							objValue = JsonValue.NULL;
						} else if (obj instanceof Boolean) {
							objValue = JsonValue.valueOf((Boolean) obj);
						} else if (obj instanceof Byte) {
							objValue = JsonValue.valueOf((Byte) obj);
						} else if (obj instanceof Short) {
							objValue = JsonValue.valueOf((Short) obj);
						} else if (obj instanceof Integer) {
							objValue = JsonValue.valueOf((Integer) obj);
						} else if (obj instanceof Long) {
							objValue = JsonValue.valueOf((Long) obj);
						} else if (obj instanceof Float) {
							objValue = JsonValue.valueOf((Float) obj);
						} else if (obj instanceof Double) {
							objValue = JsonValue.valueOf((Double) obj);
						} else if (obj instanceof Date) {
							objValue = JsonValue.valueOf(Dates.formatDate(
									(Date) obj, "dd.MM.yyyy HH:mm:ss"));
						} else {
							objValue = JsonValue.valueOf(obj.toString());
						}
						row.add(objValue);
					}
					array.add(row);
				}
				resultValue = array;
				resultType = JsonValue.valueOf("set");
			} else if (res instanceof IQueryResultSingleInteger) {
				final IQueryResultSingleInteger resInt = (IQueryResultSingleInteger) res;
				resultValue = JsonValue.valueOf(resInt.getResult());
				resultType = JsonValue.valueOf("value");
			} else {
				resultValue = JsonValue.NULL;
				resultType = JsonValue.valueOf("empty");
			}

			final JsonObject resultObject = new JsonObject();
			resultObject.add("type", resultType);
			resultObject.add("result", resultValue);
			results.add(resultObject);
		}

		if (singleQuery && results.size() == 1) {
			return results.get(0).toString();
		} else {
			return results.toString();
		}
	}
}
