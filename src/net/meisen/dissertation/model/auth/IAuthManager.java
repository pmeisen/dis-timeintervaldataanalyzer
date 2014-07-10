package net.meisen.dissertation.model.auth;

import java.util.Collection;

import net.meisen.dissertation.exceptions.AuthException;
import net.meisen.dissertation.exceptions.AuthManagementException;
import net.meisen.dissertation.exceptions.PermissionException;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;

public interface IAuthManager {

	/**
	 * Method called by the framework when the auth-manager is initialized, i.e.
	 * prior to it's first usage. The auth-manager should throw an exception if
	 * it's used in any invalid way prior to initialization. The init-method
	 * should use the provided configuration for initialization.
	 */
	public void init();

	/**
	 * Method called by the framework when the auth-manager is released, i.e.
	 * not used anymore. After releasing all authentication requests should be
	 * rejected.
	 */
	public void release();

	/**
	 * Sets the configuration to be used by the auth-manager.
	 * 
	 * @param config
	 *            the configuration to be used, can be {@code null} if the
	 *            default configuration should be used
	 */
	public void setConfig(final IAuthManagerConfig config);

	/**
	 * Logs the specified user in. If the login fails (e.g. because of invalid
	 * credentials) an {@code AuthException} has to be thrown. If not exception
	 * is thrown, the system assumes a valid login.
	 * 
	 * @param username
	 *            the name of the user to login
	 * @param password
	 *            the password of the user
	 * 
	 * @throws AuthException
	 *             if the login fails
	 */
	public void login(final String username, final String password)
			throws AuthException;

	/**
	 * Logs the current user out.
	 */
	public void logout();

	/**
	 * Checks if the {@code AuthManager} can be managed considering the users.
	 * If the method returns {@code false} all the methods of the
	 * {@code AuthManager} adding, modifying, deleting, granting or revoking
	 * users, permissions or roles should not be called, because the usage is
	 * not guaranteed to be valid.
	 * 
	 * @return {@code true} if the {@code AuthManager} is manageable, otherwise
	 *         {@code false}
	 */
	public boolean isManageable();

	/**
	 * Adds a user to the {@code AuthManager}. The method should only be called
	 * if {@link #isManageable()} returns {@code true}, otherwise the
	 * functionality is not guaranteed.
	 * 
	 * @param name
	 *            the name of the user to be added
	 * @param password
	 *            the password of the user to be added
	 * @param roles
	 *            the roles, which should be assigned to the user; can be
	 *            {@code null} if no roles should be assigned
	 * @param permissions
	 *            the permissions, which should be assigned to the user; can be
	 *            {@code null} if no permissions should be assigned
	 * 
	 * @throws AuthManagementException
	 *             if a user with the specified {@code name} already exists
	 * @throws PermissionException
	 *             if the current user is not allowed to add an user
	 */
	public void addUser(final String name, final String password,
			final String[] roles, final DefinedPermission[] permissions)
			throws AuthManagementException, PermissionException;

	/**
	 * Deletes the specified user.
	 * 
	 * @param name
	 *            the name of the user to be deleted
	 * 
	 * @throws AuthManagementException
	 *             if the deletion failed of any reason
	 * @throws PermissionException
	 *             if the current user is not allowed to delete an user
	 */
	public void deleteUser(final String name) throws AuthManagementException,
			PermissionException;

	public void modifyPassword(final String name, final String password)
			throws AuthManagementException, PermissionException;

	public void addRole(final String role, final DefinedPermission[] permissions)
			throws AuthManagementException, PermissionException;

	public void deleteRole(final String role) throws AuthManagementException,
			PermissionException;

	public void assignRoleToUser(final String username, final String role)
			throws AuthManagementException, PermissionException;

	public void removeRoleFromUser(final String username, final String role)
			throws AuthManagementException, PermissionException;

	public void grantPermissionsToUser(final String username,
			final String[] permissions) throws AuthManagementException,
			PermissionException;

	public void revokePermissionsFromUser(final String username,
			final String[] permissions) throws AuthManagementException,
			PermissionException;

	public void grantPermissionsToRole(final String role,
			final String[] permissions) throws AuthManagementException,
			PermissionException;

	public void revokePermissionsFromRole(final String role,
			final String[] permissions) throws AuthManagementException,
			PermissionException;

	public boolean hasPermission(final DefinedPermission permission);
}
