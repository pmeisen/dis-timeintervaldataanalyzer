package net.meisen.dissertation.server;

import java.io.File;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.PermissionException;
import net.meisen.dissertation.exceptions.QueryServletException;
import net.meisen.dissertation.impl.dataretriever.CsvDataConfig;
import net.meisen.dissertation.impl.dataretriever.CsvDataRetriever;
import net.meisen.dissertation.impl.dataretriever.DbConnectionConfig;
import net.meisen.dissertation.impl.dataretriever.DbDataRetriever;
import net.meisen.dissertation.impl.dataretriever.DbQueryConfig;
import net.meisen.dissertation.impl.datasets.DataRetrieverDataSet;
import net.meisen.dissertation.impl.parser.query.Interval;
import net.meisen.dissertation.impl.parser.query.insert.InsertQuery;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.parser.query.select.SelectResult;
import net.meisen.dissertation.jdbc.protocol.DataType;
import net.meisen.dissertation.jdbc.protocol.QueryType;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.dissertation.model.auth.permissions.Permission;
import net.meisen.dissertation.model.data.DataStructure;
import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry;
import net.meisen.dissertation.model.datastructure.MetaStructureEntry;
import net.meisen.dissertation.model.datastructure.StructureEntry;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.indexes.datarecord.IDataRecordMeta;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IQueryFactory;
import net.meisen.dissertation.model.parser.query.IQueryResult;
import net.meisen.dissertation.model.parser.query.IQueryResultSet;
import net.meisen.dissertation.model.parser.query.IQueryResultSingleInteger;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.dissertation.model.time.timeline.TimelineDefinition;
import net.meisen.dissertation.server.sessions.SessionManager;
import net.meisen.general.genmisc.types.Dates;
import net.meisen.general.genmisc.types.Files;
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
			exceptionRegistry.throwRuntimeException(
					QueryServletException.class, 1000, method);
			return null;
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
				return getPermissions(parameters);
			} else if ("user".equals(o)) {
				return getUserPermission(parameters);
			} else if ("role".equals(o)) {
				return getRolePermissions(parameters);
			} else if ("models".equals(o)) {
				return getModels(parameters);
			} else if ("modelmeta".equals(o)) {
				return getModelMetaData(parameters);
			} else if ("addmodelrecords".equals(o)) {
				return addRecordsFromModel(parameters);
			} else if ("adddbrecords".equals(o)) {
				return addRecordsFromDb(parameters);
			} else if ("addcsvfile".equals(o)) {
				return addRecordsFromCsv(parameters);
			} else if ("timeout".equals(o)) {
				return performTimeout(parameters);
			} else {
				exceptionRegistry.throwRuntimeException(
						QueryServletException.class, 1001, o);
				return null;
			}
		} else {
			exceptionRegistry.throwRuntimeException(
					QueryServletException.class, 1002);
			return null;
		}
	}

	/**
	 * Helper method used for testing purposes only. The method waits until a
	 * response is sent.
	 * 
	 * @param parameters
	 *            the parameters specifying the <i>timeout</i>
	 * 
	 * @return the timeout as JSON-value (stringified)
	 */
	protected String performTimeout(final Map<String, String> parameters) {

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
			try {
				Thread.sleep(timeout);
			} catch (final InterruptedException e) {
				throw new IllegalStateException();
			}
		} else {
			timeout = 0;
		}

		// return the time waited
		final JsonValue value = JsonValue.valueOf(timeout);
		return value.toString();
	}

	/**
	 * Gets the permissions available on server-side.
	 * 
	 * @param parameters
	 *            the parameters specifying details of the request
	 * 
	 * @return a JSON-array (stringified) containing all permissions
	 */
	protected String getPermissions(final Map<String, String> parameters) {

		// check the permission to use this system retrieval
		if (!authManager.hasPermission(Permission.manageUsers.create())) {
			exceptionRegistry.throwRuntimeException(PermissionException.class,
					1000, Permission.manageUsers);
		}

		// get the permissions
		final JsonArray array = new JsonArray();
		for (final String p : Permission.transform(Permission.values(), "*",
				DefinedPermission.DEF_SEPARATOR)) {
			array.add(p);
		}

		// return the permissions
		return array.toString();
	}

	/**
	 * Gets the permissions of the current user. The parameters must contain a
	 * <i>username</i> and may contain a boolean named by <i>includeRole</i>,
	 * specifying if roles should be contained in the reply
	 * 
	 * @param parameters
	 *            the parameters specifying the data to be retrieved
	 * 
	 * @return an JSON-object (stringified) containing the <i>permissions</i>
	 *         and <i>roles</i> of the current user, as well as it's
	 *         <i>username</i>
	 */
	private String getUserPermission(final Map<String, String> parameters) {

		// check the permission to use this system retrieval
		if (!authManager.hasPermission(Permission.manageUsers.create())) {
			exceptionRegistry.throwRuntimeException(PermissionException.class,
					1000, Permission.manageUsers);
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
	}

	/**
	 * Gets the permissions of a specific role available on the server.
	 * 
	 * @param parameters
	 *            the parameters specifying what to response, must contain a
	 *            <i>rolename</i> information, specifying the role to retrieve
	 *            the permissions for
	 * 
	 * @return an JSON-object (stringified) with the requested information, i.e.
	 *         an array of <i>permissions</i> and the rolename
	 */
	protected String getRolePermissions(final Map<String, String> parameters) {

		// check the permission to use this system retrieval
		if (!authManager.hasPermission(Permission.manageUsers.create())) {
			exceptionRegistry.throwRuntimeException(PermissionException.class,
					1000, Permission.manageUsers);
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
	}

	/**
	 * Method to retrieve the models of the server.
	 * 
	 * @param parameters
	 *            the parameters of the request
	 * 
	 * @return an JSON-array (stringified) specifying the models of the server
	 */
	protected String getModels(final Map<String, String> parameters) {

		// check the permission to use this system retrieval
		if (!authManager.hasPermission(Permission.load.create())) {
			exceptionRegistry.throwRuntimeException(PermissionException.class,
					1000, Permission.load);
		}

		final Set<String> loaded = handler.getTidaModels();
		final Set<String> available = handler.getAvailableTidaModels();
		final Set<String> autoloaded = handler.getAutoloadedTidaModels();
		final JsonArray array = new JsonArray();
		for (final String model : available) {
			final JsonObject object = new JsonObject();
			object.add("model", model);
			object.add("loaded", loaded.contains(model));
			object.add("autoloaded", autoloaded.contains(model));

			array.add(object);
		}

		return array.toString();
	}

	/**
	 * Gets the meta-data of the model.
	 * 
	 * @param parameters
	 *            the parameters specifying the data to be retrieved
	 * 
	 * @return a JSON string specifying the requested meta-data
	 */
	protected String getModelMetaData(final Map<String, String> parameters) {

		// get the meta-data of the model
		final TidaModel model = getModel(parameters, QueryType.QUERY);

		// create the JSON instance of the meta information
		final JsonArray res = new JsonArray();
		final IDataRecordMeta meta = model.getDataRecordFactory().getMeta();
		final String[] names = meta.getNames();
		final DataType[] types = meta.getDataTypes();
		final int len = names.length;
		for (int i = 0; i < len; i++) {
			final int pos = i + 1;
			final JsonObject entry = new JsonObject();

			// determine the meta-type
			final String metaType;
			if (pos == meta.getPosRecordId()) {
				metaType = "ID";
			} else if (pos == meta.getPosStart()) {
				metaType = "START";
			} else if (pos == meta.getPosEnd()) {
				metaType = "END";
			} else if (pos >= meta.getFirstPosDescModelIds()
					&& pos <= meta.getLastPosDescModelIds()) {
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
	}

	/**
	 * Adds the records of the specified CSV-file to the specified model. The
	 * parameters must contain:
	 * <ul>
	 * <li><b>sessionId</b> - the identifier of the session</li>
	 * <li><b>model</b> - a string specifying the model's identifier</li>
	 * <li><b>file</b> - the name of the CSV-file as defined within the session
	 * (see {@link SessionManager#getFile(String, String)}).</li>
	 * <li><b>separator</b> - the separator used within the CSV-file
	 * <li><b>structure</b> - a JSON string, specifying an array of
	 * <i>descriptor</i>, <i>column</i> or <i>interval</i>, <i>column</i> pairs</li>
	 * </ul>
	 * 
	 * @param parameters
	 *            the parameters to determine the CSV-file and model from
	 * 
	 * @return a {@code TRUE} JSON-string specifying that the adding was
	 *         successful
	 */
	protected String addRecordsFromCsv(final Map<String, String> parameters) {

		// get the model
		final TidaModel model = getModel(parameters, QueryType.MANIPULATION);

		// get the csv settings
		final CsvDataConfig config = new CsvDataConfig();

		final String sessionId = parameters.get(PARAM_SESSIONID);
		final File file = sessionManager.getFile(sessionId,
				parameters.get("file"));
		config.setFile(Files.getCanonicalPath(file));
		config.setSeparator(parameters.get("separator"));

		// get the structure
		final DataStructure structure = getStructure("column", parameters);

		// get the data
		CsvDataRetriever retriever = null;
		try {

			// get the retriever
			retriever = new CsvDataRetriever(UUID.randomUUID().toString(),
					config);
			retriever.setExceptionRegistry(exceptionRegistry);

			// use it to load data
			final DataRetrieverDataSet dataSet = new DataRetrieverDataSet(
					retriever, null);

			// do the loading
			model.bulkLoadData(structure, dataSet.iterator());
		} finally {

			// release all resources
			if (retriever != null) {
				retriever.release();
			}
		}

		// just return a true, it is loaded
		return JsonValue.TRUE.toString();
	}

	/**
	 * Adds the records of the specified database to the specified model. The
	 * parameters must contain:
	 * <ul>
	 * <li><b>model</b> - a strign specifying the model's identifier</li>
	 * <li><b>connection</b> - a JSON string specifying an object with
	 * attributes <i>driver</i>, <i>username</i>, <i>password</i>, and
	 * <i>url</i></li>
	 * <li><b>query</b> - the query to be fired against the database
	 * <li><b>structure</b> - a JSON string, specifying an array of
	 * <i>descriptor</i>, <i>dbname</i> or <i>interval</i>, <i>dbname</i> pairs</li>
	 * </ul>
	 * 
	 * @param parameters
	 *            the parameters to determine the data of the database and model
	 *            from
	 * 
	 * @return a {@code TRUE} JSON-string specifying that the adding was
	 *         successful
	 */
	protected String addRecordsFromDb(final Map<String, String> parameters) {

		// get the model
		final TidaModel model = getModel(parameters, QueryType.MANIPULATION);

		// get the database connection and the query to be used
		final DbConnectionConfig config = getDatabaseConfig(parameters);
		final DbQueryConfig query = getDatabaseQuery(parameters);

		// get the structure
		final DataStructure structure = getStructure("dbname", parameters);

		// get the data
		DbDataRetriever retriever = null;
		try {

			// get the retriever
			retriever = new DbDataRetriever(UUID.randomUUID().toString(),
					config);
			retriever.setExceptionRegistry(exceptionRegistry);

			// use it to load data
			final DataRetrieverDataSet dataSet = new DataRetrieverDataSet(
					retriever, query);

			// do the loading
			model.bulkLoadData(structure, dataSet.iterator());
		} finally {

			// release all db resources
			if (retriever != null) {
				retriever.release();
			}
		}

		// just return a true, it is loaded
		return JsonValue.TRUE.toString();
	}

	/**
	 * Method used to add the data specified within the {@code TidaModel}.
	 * 
	 * @param parameters
	 *            the parameters used to get the model
	 * 
	 * @return a {@code TRUE} JSON-string specifying that the adding was
	 *         successful
	 */
	protected String addRecordsFromModel(final Map<String, String> parameters) {

		// execute the loading of the data
		final TidaModel model = getModel(parameters, QueryType.MANIPULATION);

		// execute the loading of the model's data
		model.bulkLoadDataFromDataModel();

		// just return a true, it is loaded
		return JsonValue.TRUE.toString();
	}

	/**
	 * Helper method used to retrieve the specified structure (<i>structure</i>
	 * JSON-object) from the parameters.
	 * 
	 * @param fieldName
	 *            the name of the attribute within the JSON-object to read the
	 *            value from
	 * @param parameters
	 *            the parameters to read the structure from
	 * 
	 * @return the read structure
	 */
	protected DataStructure getStructure(final String fieldName,
			final Map<String, String> parameters) {

		// get the structure
		final JsonValue parameter = JsonValue.readFrom(parameters
				.get("structure"));
		if (parameter == null || !parameter.isArray()) {
			exceptionRegistry.throwRuntimeException(
					QueryServletException.class, 1003, "structure", parameter);
			return null;
		}
		final JsonArray values = (JsonArray) parameter;

		final List<StructureEntry> entries = new ArrayList<StructureEntry>();
		for (final JsonValue v : values) {
			if (v.isObject()) {
				final JsonObject jsonEntry = (JsonObject) v;
				final StructureEntry entry;

				// get the values
				final String value = get(jsonEntry, fieldName);
				final String descriptor = get(jsonEntry, "descriptor", true);
				final String interval = get(jsonEntry, "interval", true);

				// evaluate the values
				if (descriptor == null && interval == null) {
					exceptionRegistry.throwRuntimeException(
							QueryServletException.class, 1003,
							"descriptor/interval", null);
					return null;
				} else if (descriptor != null) {
					entry = new MetaStructureEntry(descriptor, value);
				} else if (interval != null) {
					entry = new IntervalStructureEntry(interval, value);
				} else {
					// impossible
					throw new IllegalStateException("Dead code reached");
				}

				entries.add(entry);
			} else {
				exceptionRegistry.throwRuntimeException(
						QueryServletException.class, 1003, "structure",
						parameter);
				return null;
			}
		}
		return new DataStructure(entries);
	}

	/**
	 * Helper method to get the string-value of the specified attribute (by
	 * {@code name}) from the specified {@code JsonObject}.
	 * 
	 * @param obj
	 *            the object to read the value from
	 * @param name
	 *            the name of the attribute to be read
	 * 
	 * @return the string value of the attribute specified
	 */
	protected String get(final JsonObject obj, final String name) {
		return get(obj, name, false);
	}

	/**
	 * Helper method to get the string-value of the specified attribute (by
	 * {@code name}) from the specified {@code JsonObject}.
	 * 
	 * @param obj
	 *            the object to read the value from
	 * @param name
	 *            the name of the attribute to be read
	 * @param optional
	 *            {@code true} if the value is optional, i.e. {@code null} may
	 *            be returned; otherwise {@code false}
	 * 
	 * @return the string value of the attribute specified, may be {@code null}
	 *         if and only if the attribute does not exist
	 */
	protected String get(final JsonObject obj, final String name,
			final boolean optional) {
		final JsonValue val = obj.get(name);
		if (val == null) {
			if (optional) {
				return null;
			} else {
				exceptionRegistry.throwRuntimeException(
						QueryServletException.class, 1004, name, obj);
				return null;
			}
		} else {
			try {
				return val.asString();
			} catch (final UnsupportedOperationException e) {
				exceptionRegistry.throwRuntimeException(
						QueryServletException.class, 1005, name, val);
				return null;
			}
		}
	}

	/**
	 * Reads a database configuration, specified by the <i>connection</i>
	 * parameter, from the passed {@code parameters}. The connection must be
	 * specified as a stringified JSON-object containing the attributes:
	 * <ul>
	 * <li><i>driver</i> - driver class</li>,
	 * <li><i>username</i> - the name of the user</li>,
	 * <li><i>password</i> - the user's password</li>, and
	 * <li><i>url</i> - the JDBC-url</li>
	 * </ul>
	 * 
	 * @param parameters
	 *            the parameters to read from
	 * 
	 * @return the read connection
	 */
	protected DbConnectionConfig getDatabaseConfig(
			final Map<String, String> parameters) {

		// get the database connection
		final DbConnectionConfig config = new DbConnectionConfig();
		final JsonValue value = JsonValue
				.readFrom(parameters.get("connection"));
		if (!value.isObject()) {
			exceptionRegistry.throwRuntimeException(
					QueryServletException.class, 1006, "connection", value);
			return null;
		}
		final JsonObject jsonConfig = (JsonObject) value;
		config.setDriver(get(jsonConfig, "driver"));
		config.setUsername(get(jsonConfig, "username"));
		config.setPassword(get(jsonConfig, "password"));
		config.setUrl(get(jsonConfig, "url"));

		return config;
	}

	/**
	 * Reads a query, specified by the <i>query</i> parameter, from the passed
	 * {@code parameters}. The query must be specified as string.
	 * 
	 * @param parameters
	 *            the parameters to read from
	 * 
	 * @return the read query
	 */
	protected DbQueryConfig getDatabaseQuery(
			final Map<String, String> parameters) {

		final String query = parameters.get("query");
		if (query == null || query.isEmpty()) {
			exceptionRegistry.throwRuntimeException(
					QueryServletException.class, 1004, "query", parameters);
			return null;
		}

		// get the query to be used
		final DbQueryConfig queryConfig = new DbQueryConfig();
		queryConfig.setLanguage("SQL");
		queryConfig.setQuery(query);

		return queryConfig;
	}

	/**
	 * Reads the model and checks the permissions based on the specified
	 * {@code type}.
	 * 
	 * @param parameters
	 *            the parameters to determine the model from
	 * @param type
	 *            the type to check the permissions for
	 * 
	 * @return the read model
	 */
	protected TidaModel getModel(final Map<String, String> parameters,
			final QueryType type) {
		final String modelId = parameters.get("model");

		// define the type of permissions needed
		final DefinedPermission[][] permSets;
		if (QueryType.QUERY.equals(type)) {
			permSets = new DefinedPermission[][] {
					new DefinedPermission[] { Permission.query.create(modelId) },
					new DefinedPermission[] { Permission.queryAll.create() } };
		} else if (QueryType.MANIPULATION.equals(type)) {
			permSets = new DefinedPermission[][] {
					new DefinedPermission[] { Permission.modify.create(modelId) },
					new DefinedPermission[] { Permission.modifyAll.create() } };
		} else {
			exceptionRegistry.throwRuntimeException(
					QueryServletException.class, 1007, type);
			return null;
		}

		// check the permission to use this system retrieval
		if (!DefinedPermission.checkPermission(authManager, permSets)) {
			exceptionRegistry.throwRuntimeException(PermissionException.class,
					1000, DefinedPermission.toString(permSets));
		}

		// execute the loading of the data
		final TidaModel model = handler.getTidaModel(modelId);
		if (model == null) {
			exceptionRegistry.throwRuntimeException(
					QueryServletException.class, 1006, modelId);
			return null;
		}

		return model;
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

			// the values to be determined
			final JsonValue resultValue, resultType, resultNames;

			// some additional values which might be added
			JsonValue resultAdd = null;

			// get the values depending on the queryResult
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
						row.add(createJsonValue(obj));
					}
					array.add(row);
				}
				resultValue = array;
				resultNames = names;
				resultType = JsonValue.valueOf("set");

				// we add some additional information
				if (resQuery instanceof SelectResult) {
					final SelectQuery selQuery = ((SelectResult) resQuery)
							.getQuery();

					// create some information about the query itself
					final JsonObject queryInfo = createQueryInformation(selQuery);

					// add the query information to the additional info
					resultAdd = new JsonObject();
					((JsonObject) resultAdd).add("query", queryInfo);
				}

			} else if (resQuery instanceof IQueryResultSingleInteger) {
				final IQueryResultSingleInteger resInt = (IQueryResultSingleInteger) resQuery;

				if (isInsertQuery) {
					final JsonArray res = new JsonArray();
					res.add(resInt.getResult());
					res.add(intArrayToJson(resInt.getCollectedIds()));

					resultValue = res;
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
			if (resultAdd != null) {
				resultObject.add("additional", resultAdd);
			}
			results.add(resultObject);
		}

		if (singleQuery && results.size() == 1) {
			return results.get(0).toString();
		} else {
			return results.toString();
		}
	}

	/**
	 * Creates a JSON with query information.
	 * 
	 * @param selQuery
	 *            the query to create the information for
	 * 
	 * @return the created JSON
	 */
	protected JsonObject createQueryInformation(final SelectQuery selQuery) {
		final TidaModel model = this.handler
				.getTidaModel(selQuery.getModelId());
		final IntervalModel intervalModel = model.getIntervalModel();
		final TimelineDefinition timeline = intervalModel
				.getTimelineDefinition();
		final BaseMapper<?> mapper = intervalModel.getTimelineMapper();
		final Interval<?> interval = selQuery.getInterval();
		final JsonObject queryInfo = new JsonObject();

		// add the interval's type
		final JsonObject intInfo = new JsonObject();
		if (interval != null) {
			if (Number.class.isAssignableFrom(interval.getType())) {
				intInfo.add("type", "number");
			} else if (Date.class.isAssignableFrom(interval.getType())) {
				intInfo.add("type", "date");
			}
	
			// add the interval's start- and end-value
			final long[] bounds = mapper.getBounds(interval);
			intInfo.add("raw-start", createJsonValue(interval.getStart()));
			intInfo.add("raw-end", createJsonValue(interval.getEnd()));
			intInfo.add("start", createJsonValue(mapper.resolve(bounds[0])));
			intInfo.add("end", createJsonValue(mapper.resolve(bounds[1])));
		}

		// add the axis's start- and end-value
		final JsonObject axisInfo = new JsonObject();
		if (timeline != null) {
			if (Number.class.isAssignableFrom(timeline.getType())) {
				axisInfo.add("type", "number");
			} else if (Date.class.isAssignableFrom(timeline.getType())) {
				axisInfo.add("type", "date");
			}
			axisInfo.add("start", createJsonValue(timeline.getStart()));
			axisInfo.add("end", createJsonValue(timeline.getEnd()));
			axisInfo.add("granularity", timeline.getGranularity().toString());
		}

		// add all the info to the query info
		queryInfo.add("interval", intInfo);
		queryInfo.add("timeaxis", axisInfo);
		queryInfo.add("model", selQuery.getModelId());

		return queryInfo;
	}

	/**
	 * Helper method to create a {@code JsonValue} for the specified {@code obj}
	 * .
	 * 
	 * @param obj
	 *            the object to create a JSON representative for
	 * 
	 * @return the created representative
	 */
	protected JsonValue createJsonValue(final Object obj) {
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
			final Float f = (Float) obj;
			if (Float.isNaN(f) || Float.isInfinite(f)) {
				objValue = JsonValue.NULL;
			} else {
				objValue = JsonValue.valueOf((Float) obj);
			}
		} else if (obj instanceof Double) {
			final Double d = (Double) obj;
			if (Double.isNaN(d) || Double.isInfinite(d)) {
				objValue = JsonValue.NULL;
			} else {
				objValue = JsonValue.valueOf((Double) obj);
			}
		} else if (obj instanceof Date) {
			objValue = JsonValue.valueOf(Dates.formatDate((Date) obj,
					"dd.MM.yyyy HH:mm:ss"));
		} else {
			objValue = JsonValue.valueOf(obj.toString());
		}

		return objValue;
	}

	/**
	 * Transforms an int-array to a JSON-array.
	 * 
	 * @param array
	 *            the array to be transformed
	 * 
	 * @return the JSON-array
	 */
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
