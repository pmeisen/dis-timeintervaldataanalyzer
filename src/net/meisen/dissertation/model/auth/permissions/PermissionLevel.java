package net.meisen.dissertation.model.auth.permissions;

/**
 * The instance defines different levels of permissions.
 * 
 * @author pmeisen
 * 
 */
public enum PermissionLevel {
	/**
	 * The permission is independent of everything, it is globally set.
	 */
	GLOBAL,
	/**
	 * The permission is set for a model only. It thereby has to be applied for
	 * each and every model.
	 */
	MODEL;
}
