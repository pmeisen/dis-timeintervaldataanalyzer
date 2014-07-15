package net.meisen.dissertation.model.auth;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.AuthException;
import net.meisen.dissertation.exceptions.AuthManagementException;
import net.meisen.dissertation.exceptions.PermissionException;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;

/**
 * An authentication manager is used to check authentications and permissions.
 * The manager is available using auto-wiring with a qualifier defined by
 * {@link DefaultValues#AUTHMANAGER_ID}.
 * 
 * @author pmeisen
 * 
 */
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
	 *             if the current user is not allowed to add a user
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
	 *             if the current user is not allowed to delete a user
	 */
	public void deleteUser(final String name) throws AuthManagementException,
			PermissionException;

	/**
	 * Modifies the password of the specified user to the {@code password}.
	 * 
	 * @param name
	 *            the name of the user to be modified
	 * @param password
	 *            the new password
	 * 
	 * @throws AuthManagementException
	 *             if the password cannot be modified because of an error
	 * @throws PermissionException
	 *             if the current user is not allowed to modify a user
	 */
	public void modifyPassword(final String name, final String password)
			throws AuthManagementException, PermissionException;

	/**
	 * Adds a role to the manager.
	 * 
	 * @param role
	 *            the name of the role
	 * @param permissions
	 *            the permissions granted to the role, can be {@code null} if no
	 *            permissions are granted
	 * 
	 * @throws AuthManagementException
	 *             if the deletion failed of any reason
	 * @throws PermissionException
	 *             if the current user is not allowed to delete a role
	 */
	public void addRole(final String role, final DefinedPermission[] permissions)
			throws AuthManagementException, PermissionException;

	/**
	 * Deletes a role from the manager.
	 * 
	 * @param role
	 *            the name of the role to be deleted
	 * 
	 * @throws AuthManagementException
	 *             if the adding failed, e.g. because another role with the same
	 *             name is already added to the manager
	 * @throws PermissionException
	 *             if the current user is not allowed to delete a role
	 */
	public void deleteRole(final String role) throws AuthManagementException,
			PermissionException;

	/**
	 * Assigns the specified {@code role} to the specified {@code username}. The
	 * user must exists, whereby the role may not exist when it is assigned.
	 * 
	 * @param username
	 *            the name of the user to assign the specified {@code role} to
	 * @param role
	 *            the role to be assigned (might not exist so far)
	 * 
	 * @throws AuthManagementException
	 *             if the user does not exist or if the assignment fails because
	 *             of another reason
	 * @throws PermissionException
	 *             if the current user is not allowed to assign a role
	 */
	public void assignRoleToUser(final String username, final String role)
			throws AuthManagementException, PermissionException;

	/**
	 * Removes the specified {@code role} from the specified {@code username}.
	 * 
	 * @param username
	 *            the name of the user to remove the role from
	 * @param role
	 *            the role to be removed
	 * 
	 * @throws AuthManagementException
	 *             if the user does not exist
	 * @throws PermissionException
	 *             if the current user is not allowed to remove a role
	 */
	public void removeRoleFromUser(final String username, final String role)
			throws AuthManagementException, PermissionException;

	/**
	 * Grants the specified {@code permissions} to the specified
	 * {@code username}.
	 * 
	 * @param username
	 *            the user to grant the {@code permissions} to
	 * @param permissions
	 *            the permissions to be granted
	 * 
	 * @throws AuthManagementException
	 *             if the user does not exist or if the {@code permissions} to
	 *             be granted are empty or {@code null} or if the granting fails
	 *             because of another problem
	 * @throws PermissionException
	 *             if the current user is not allowed to grant permissions
	 */
	public void grantPermissionsToUser(final String username,
			final String[] permissions) throws AuthManagementException,
			PermissionException;

	/**
	 * Revokes the specified {@code permissions} from the specified
	 * {@code username}.
	 * 
	 * @param username
	 *            the user to revoke the {@code permissions} from
	 * @param permissions
	 *            the permissions to be revoked
	 * 
	 * @throws AuthManagementException
	 *             if the user does not exist or if the revoking fails because
	 *             of another problem
	 * @throws PermissionException
	 *             if the current user is not allowed to revoke permissions
	 */
	public void revokePermissionsFromUser(final String username,
			final String[] permissions) throws AuthManagementException,
			PermissionException;

	/**
	 * Grants the specified {@code permissions} to the specified {@code role}.
	 * 
	 * @param role
	 *            the role to apply the {@code permissions} to
	 * @param permissions
	 *            the permissions to be applied
	 * 
	 * @throws AuthManagementException
	 *             if the specified {@code role} does not exist, if the defined
	 *             {@code permissions} are {@code null} or empty, or if another
	 *             problem with granting the permissions occurs
	 * @throws PermissionException
	 *             if the current user is not allowed to grant permissions
	 */
	public void grantPermissionsToRole(final String role,
			final String[] permissions) throws AuthManagementException,
			PermissionException;

	/**
	 * Revokes the specified {@code permissions} from the specified {@code role}
	 * .
	 * 
	 * @param role
	 *            the role to revoke the {@code permissions} from
	 * @param permissions
	 *            the permissions to be revoked
	 * 
	 * @throws AuthManagementException
	 *             if the realm is closed, if the specified {@code role} does
	 *             not exist, or if another problem with granting the
	 *             permissions occurs
	 * @throws PermissionException
	 *             if the current user is not allowed to revoke permissions
	 */
	public void revokePermissionsFromRole(final String role,
			final String[] permissions) throws AuthManagementException,
			PermissionException;

	/**
	 * Checks if the current user has the specified {@code permission}.
	 * 
	 * @param permission
	 *            the permission to be checked
	 * 
	 * @return {@code true} if the user has the permission, otherwise
	 *         {@code false}.
	 */
	public boolean hasPermission(final DefinedPermission permission);
}
