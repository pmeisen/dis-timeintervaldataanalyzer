package net.meisen.dissertation.model.auth.permissions;

import java.util.ArrayList;
import java.util.List;

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
	 * 
	 */
	connectTSQL(PermissionLevel.GLOBAL),
	/**
	 * 
	 */
	connectHTML(PermissionLevel.GLOBAL),
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

	public PermissionLevel getLevel() {
		return level;
	}

	public String toString(final String separator) {
		return toString(null, separator);
	}

	public String toString(final String modelId, final String separator) {
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

	public DefinedPermission create() {
		return new DefinedPermission(this, null);
	}

	public DefinedPermission create(final String modelId) {
		return new DefinedPermission(this, modelId);
	}

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
