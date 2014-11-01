package net.meisen.dissertation.impl.parser.query.get;

/**
 * Enum definition of the different results possible when querying for data
 * using a get-statement.
 * 
 * @author pmeisen
 * 
 */
public enum GetResultType {
	/**
	 * The user expects a set of known {@code TidaModel} instances as result.
	 */
	MODELS,
	/**
	 * The user expects information about the version of the database.
	 */
	VERSION,
	/**
	 * The users available with their permissions.
	 */
	USERS,
	/**
	 * The available permissions for each user and model.
	 */
	PERMISSIONS,
	/**
	 * The roles available with their permissions.
	 */
	ROLES;
}
