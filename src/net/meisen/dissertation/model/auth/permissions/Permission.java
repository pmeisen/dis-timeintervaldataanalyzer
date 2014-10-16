package net.meisen.dissertation.model.auth.permissions;

import java.util.ArrayList;
import java.util.List;

/**
 * Permissions of the time-interval database implementation.
 * 
 * @author pmeisen
 * 
 */
public enum Permission {

	/**
	 * Permission to add, delete or modify (i.e. name and password) users.
	 */
	manageUsers(PermissionLevel.GLOBAL),
	/**
	 * Permission to grant permissions to (other) users.
	 */
	grantPermissions(PermissionLevel.GLOBAL),
	/**
	 * Permission to connect on the TSQL socket to fire queries against the
	 * database.
	 */
	connectTSQL(PermissionLevel.GLOBAL),
	/**
	 * Permission to connect on the HTTP socket to any services, other than
	 * login servlet.
	 */
	connectHTTP(PermissionLevel.GLOBAL),
	/**
	 * 
	 */
	queryAll(PermissionLevel.GLOBAL),
	/**
	 * 
	 */
	modifyAll(PermissionLevel.GLOBAL),
	/**
	 * 
	 */
	get(PermissionLevel.GLOBAL),
	/**
	 * 
	 */
	load(PermissionLevel.GLOBAL),
	/**
	 * 
	 */
	unload(PermissionLevel.GLOBAL),
	/**
	 * 
	 */
	query(PermissionLevel.MODEL),
	/**
	 * 
	 */
	modify(PermissionLevel.MODEL);

	private final PermissionLevel level;

	private Permission(final PermissionLevel level) {
		this.level = level;
	}

	/**
	 * Gets the level of the permission.
	 * 
	 * @return the level of the permission
	 */
	public PermissionLevel getLevel() {
		return level;
	}

	/**
	 * Creates a string representation of the {@code Permission} using the
	 * specified {@code separator} to separate the level from the permission.
	 * 
	 * @param separator
	 *            the separator to be used to separate the level from the
	 *            permission
	 * 
	 * @return the string representation of {@code this}
	 */
	public String toString(final String separator) {
		return toString(null, separator);
	}

	/**
	 * Creates a string representation of the {@code Permission} using the
	 * specified {@code separator} to separate the level from the permission.
	 * The {@link PermissionLevel#MODEL} needs additional information (i.e. the
	 * model identifier), which have to be specified if needed by {@code this}.
	 * 
	 * @param modelId
	 *            the identifier of the model the string representation is
	 *            created for, can be {@code null} if {@code this} does not need
	 *            any model identifier
	 * @param separator
	 *            the separator to be used to separate the level from the
	 *            permission
	 * 
	 * @return the string representation of {@code this}
	 * 
	 * @throws IllegalArgumentException
	 *             if the {@code modelId} is {@code null} and the level is
	 *             {@link PermissionLevel#MODEL}
	 */
	public String toString(final String modelId, final String separator)
			throws IllegalArgumentException {
		if (PermissionLevel.GLOBAL.equals(level)) {
			return level.name() + separator + name();
		} else if (modelId == null) {
			throw new IllegalArgumentException(
					"Cannot create the permission for '" + toString()
							+ "', because no model was provided.");
		} else if (PermissionLevel.MODEL.equals(level)) {
			return level.name() + separator + modelId + separator + name();
		} else {
			throw new IllegalArgumentException("Invalid level '" + level
					+ "' used for permission '" + toString() + "'.");
		}
	}

	@Override
	public String toString() {
		return level.name() + "." + name();
	}

	/**
	 * Creates a {@code DefinedPermission} instance of {@code this}.
	 * 
	 * @return the created instance
	 */
	public DefinedPermission create() {
		return new DefinedPermission(this, null);
	}

	/**
	 * Creates a {@code DefinedPermission} instance of {@code this} using the
	 * specified {@code modelId} if needed.
	 * 
	 * @param modelId
	 *            the model identifier used if needed for {@code this}
	 * 
	 * @return the created instance
	 */
	public DefinedPermission create(final String modelId) {
		return new DefinedPermission(this, modelId);
	}

	/**
	 * Checks if the {@code PermissionLevel} of {@code this} is
	 * {@link PermissionLevel#GLOBAL}.
	 * 
	 * @return {@code true} if the level is global, otherwise {@code false}
	 */
	public boolean isGlobal() {
		return PermissionLevel.GLOBAL.equals(getLevel());
	}

	/**
	 * Gets all the {@code Permissions} of the specified {@code level}.
	 * 
	 * @param level
	 *            the {@code PermissionLevel} to retrieve the values for
	 * 
	 * @return the {@code Permissions} of the specified {@code level}
	 */
	public static Permission[] values(final PermissionLevel level) {

		if (level == null) {
			return values();
		} else {

			// check all the permissions
			final List<Permission> permissions = new ArrayList<Permission>();
			for (final Permission p : values()) {
				if (level.equals(p.level)) {
					permissions.add(p);
				}
			}

			return permissions.toArray(new Permission[0]);
		}
	}

	/**
	 * Helper method used to transform a set of {@code DefinedPermission}
	 * instances into their corresponding string representation using
	 * {@link DefinedPermission#toString(String)}.
	 * 
	 * @param permissions
	 *            the permissions to be transformed
	 * @param separator
	 *            the separator used to create the representation
	 * 
	 * @return the created representations
	 */
	public static String[] transform(final DefinedPermission[] permissions,
			final String separator) {

		if (permissions == null) {
			return new String[0];
		} else {
			final String[] perms = new String[permissions.length];
			for (int i = 0; i < permissions.length; i++) {
				final DefinedPermission p = permissions[i];
				perms[i] = p.toString(separator);
			}

			return perms;
		}
	}

	/**
	 * Helper method used to transform a set of {@code Permissions} into their
	 * corresponding string representation using
	 * {@link Permission#toString(String, String)}.
	 * 
	 * @param permissions
	 *            the permissions to be transformed
	 * @param modelId
	 *            the identifier of the model used to create the representations
	 *            if needed
	 * @param separator
	 *            the separator used to create the representation
	 * 
	 * @return the created representations
	 */
	public static String[] transform(final Permission[] permissions,
			final String modelId, final String separator) {

		if (permissions == null) {
			return new String[0];
		} else {
			final String[] perms = new String[permissions.length];
			for (int i = 0; i < permissions.length; i++) {
				final Permission p = permissions[i];
				perms[i] = p.toString(modelId, separator);
			}

			return perms;
		}
	}
}
