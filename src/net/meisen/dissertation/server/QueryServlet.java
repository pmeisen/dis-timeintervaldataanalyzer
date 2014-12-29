package net.meisen.dissertation.server;

import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.PermissionException;
import net.meisen.dissertation.impl.parser.query.insert.InsertQuery;
import net.meisen.dissertation.jdbc.protocol.DataType;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.dissertation.model.auth.permissions.Permission;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.indexes.datarecord.IDataRecordMeta;
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

/**
 * Servlet used to fire TSQL queries via the HTTP connection.
 * 
 * @author pmeisen
 * 
 */
public class QueryServlet extends BaseServlet {

	@Autowired
	@Qualifier(DefaultValues.QUERYFACTORY_ID)
	private IQueryFactory queryFactory;

	@Autowired
	@Qualifier(DefaultValues.MODELHANDLER_ID)
	private TidaModelHandler handler;

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

	/**
	 * Executes a so called system-query, to retrieve system specific
	 * information or execute system specific queries not available via TSQL.
	 * 
	 * @param parameters
	 *            the parameters passed with the request
	 * 
	 * @return the result
	 * 
	 * @throws Exception
	 *             if an exception occurs
	 */
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
			} else if ("models".equals(o)) {

				// check the permission to use this system retrieval
				if (!authManager.hasPermission(Permission.load.create())) {
					exceptionRegistry.throwRuntimeException(
							PermissionException.class, 1000, Permission.load);
				}

				final Set<String> loaded = handler.getTidaModels();
				final Set<String> available = handler.getAvailableTidaModels();
				final Set<String> autoloaded = handler
						.getAutoloadedTidaModels();
				final JsonArray array = new JsonArray();
				for (final String model : available) {
					final JsonObject object = new JsonObject();
					object.add("model", model);
					object.add("loaded", loaded.contains(model));
					object.add("autoloaded", autoloaded.contains(model));

					array.add(object);
				}

				return array.toString();
			} else if ("modelmeta".equals(o)) {
				final String modelId = parameters.get("model");
				final DefinedPermission[][] permSets = new DefinedPermission[][] {
						new DefinedPermission[] { Permission.query
								.create(modelId) },
						new DefinedPermission[] { Permission.queryAll.create() } };

				// check the permission to use this system retrieval
				if (!DefinedPermission.checkPermission(authManager, permSets)) {
					exceptionRegistry.throwRuntimeException(
							PermissionException.class, 1000,
							DefinedPermission.toString(permSets));
				}

				// get the meta-data of the model
				final TidaModel model = handler.getTidaModel(modelId);
				if (model == null) {
					// TODO throw exception
					throw new IllegalStateException("A model with identifier '"
							+ modelId + "' is not loaded or available.");
				}

				// create the JSON instance of the meta information
				final JsonArray res = new JsonArray();
				final IDataRecordMeta meta = model.getDataRecordFactory()
						.getMeta();
				final String[] names = meta.getNames();
				final DataType[] types = meta.getDataTypes();
				final int len = names.length;
				for (int i = 0; i < len; i++) {
					final JsonObject entry = new JsonObject();

					// determine the meta-type
					final String metaType;
					if (i == meta.getPosRecordId()) {
						metaType = "ID";
					} else if (i == meta.getPosStart()) {
						metaType = "START";
					} else if (i == meta.getPosEnd()) {
						metaType = "END";
					} else if (i >= meta.getFirstPosDescModelIds()
							&& i <= meta.getLastPosDescModelIds()) {
						metaType = "DESCRIPTOR";
					} else {
						metaType = "UNKNOWN";
					}

					entry.add("name", names[i]);
					entry.add("datatype", types[i].name());
					entry.add("metatype", metaType);
					res.add(entry);
				}

				return res.toString();
			} else if ("addmodelrecords".equals(o)) {
				final String modelId = parameters.get("model");
				final DefinedPermission[][] permSets = new DefinedPermission[][] {
						new DefinedPermission[] { Permission.modify
								.create(modelId) },
						new DefinedPermission[] { Permission.modifyAll.create() } };

				// check the permission to use this system retrieval
				if (!DefinedPermission.checkPermission(authManager, permSets)) {
					exceptionRegistry.throwRuntimeException(
							PermissionException.class, 1000,
							DefinedPermission.toString(permSets));
				}

				// execute the loading of the data
				final TidaModel model = handler.getTidaModel(modelId);
				if (model == null) {
					// TODO throw exception
					throw new IllegalStateException("A model with identifier '"
							+ modelId + "' is not loaded or available.");
				}

				// execute the loading of the model's data
				model.bulkLoadDataFromDataModel();

				// just return a true, it is loaded
				return JsonValue.TRUE.toString();
			} else if ("timeout".equals(o)) {

				// get the timeout
				final String paramTimeout = parameters.get("timeout");
				int timeout;
				if (paramTimeout == null) {
					timeout = 1000;
				} else {
					try {
						timeout = Integer.parseInt(paramTimeout);
					} catch (final NumberFormatException e) {
						timeout = 1000;
					}
				}

				// sleep
				if (timeout > 0) {
					Thread.sleep(timeout);
				} else {
					timeout = 0;
				}

				// return the time waited
				final JsonValue value = JsonValue.valueOf(timeout);
				return value.toString();
			} else {
				throw new IllegalStateException("Unsupported object '" + o
						+ "' defined.");
			}
		} else {
			// TODO throw exception
			throw new IllegalStateException("Object is not defined.");
		}
	}

	/**
	 * Fires a TSQL-query.
	 * 
	 * @param parameters
	 *            the parameters passed with the request
	 * 
	 * @return the result
	 * 
	 * @throws Exception
	 *             if an exception occurs
	 */
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
			IQueryResult resQuery;
			boolean isInsertQuery = false;
			try {
				final IQuery parsedQuery = queryFactory.parseQuery(query);

				// we want to retrieve the inserted identifiers for inserts
				isInsertQuery = parsedQuery instanceof InsertQuery;
				if (isInsertQuery) {
					parsedQuery.enableIdCollection(true);
				}
				resQuery = queryFactory.evaluateQuery(parsedQuery,
						sessionManager);
			} catch (final Exception e) {
				if (failOnFailure) {
					throw e;
				} else {
					resQuery = null;
				}
			}

			// check if we have a res
			final JsonValue resultValue;
			final JsonValue resultType;
			final JsonValue resultNames;
			if (resQuery instanceof IQueryResultSet) {

				// iterate over the records and add those
				final IQueryResultSet resSet = (IQueryResultSet) resQuery;
				final JsonArray names = new JsonArray();
				for (final String name : resSet.getNames()) {
					names.add(name);
				}

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
				resultNames = names;
				resultType = JsonValue.valueOf("set");
			} else if (resQuery instanceof IQueryResultSingleInteger) {
				final IQueryResultSingleInteger resInt = (IQueryResultSingleInteger) resQuery;

				if (isInsertQuery) {
					final JsonArray res = new JsonArray();
					res.add(resInt.getResult());
					res.add(intArrayToJson(resInt.getCollectedIds()));

					resultValue = JsonValue.valueOf(resInt.getResult());
					resultType = JsonValue.valueOf("array");
					resultNames = new JsonArray().add("count").add(
							"identifiers");
				} else {
					resultValue = JsonValue.valueOf(resInt.getResult());
					resultType = JsonValue.valueOf("value");
					resultNames = new JsonArray().add("value");
				}
			} else {
				resultNames = JsonValue.NULL;
				resultValue = JsonValue.NULL;
				resultType = JsonValue.valueOf("empty");
			}

			final JsonObject resultObject = new JsonObject();
			resultObject.add("names", resultNames);
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

	protected JsonValue intArrayToJson(final int[] array) {
		if (array == null) {
			return JsonValue.NULL;
		} else {
			final JsonArray jsonArray = new JsonArray();
			for (final int val : array) {
				jsonArray.add(val);
			}

			return jsonArray;
		}
	}
}
