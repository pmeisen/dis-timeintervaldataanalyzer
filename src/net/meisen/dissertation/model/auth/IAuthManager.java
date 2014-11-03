package net.meisen.dissertation.model.auth;

import java.util.Set;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.AuthException;
import net.meisen.dissertation.exceptions.AuthManagementException;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.dissertation.server.sessions.Session;

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
	 * credentials) an {@code AuthException} has to be thrown. If no exception
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
	 * Binds the user associated to the specified {@code sessionId} to the
	 * current thread. In a multi-thread environment, each thread has to call
	 * this method once prior to accessing any auth-information. Additionally,
	 * the method has to be called right after a valid login, so that the
	 * sessionId is associated to the logged in subject.
	 * 
	 * @param session
	 *            the {@code Session} to bind the manager to
	 * 
	 * @throws AuthException
	 *             if the current subject cannot be bound to the session
	 */
	public void bind(final Session session) throws AuthException;

	/**
	 * Unbinds any bounded authentication from the current thread. This method
	 * is called in an environment, where the user authenticates itself once and
	 * is reused across different threads. Each thread has to unbind itself,
	 * otherwise a memory leak may be possible.
	 * 
	 * @throws AuthException
	 *             if the unbinding fails
	 */
	public void unbind() throws AuthException;

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
	 */
	public void addUser(final String name, final String password,
			final String[] roles, final DefinedPermission[] permissions)
			throws AuthManagementException;

	/**
	 * Deletes the specified user.
	 * 
	 * @param name
	 *            the name of the user to be deleted
	 * 
	 * @throws AuthManagementException
	 *             if the deletion failed of any reason
	 */
	public void deleteUser(final String name) throws AuthManagementException;

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
	 */
	public void modifyPassword(final String name, final String password)
			throws AuthManagementException;

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
	 */
	public void addRole(final String role, final DefinedPermission[] permissions)
			throws AuthManagementException;

	/**
	 * Deletes a role from the manager.
	 * 
	 * @param role
	 *            the name of the role to be deleted
	 * 
	 * @throws AuthManagementException
	 *             if the adding failed, e.g. because another role with the same
	 *             name is already added to the manager
	 */
	public void deleteRole(final String role) throws AuthManagementException;

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
	 */
	public void assignRoleToUser(final String username, final String role)
			throws AuthManagementException;

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
	 */
	public void removeRoleFromUser(final String username, final String role)
			throws AuthManagementException;

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
	 */
	public void grantPermissionsToUser(final String username,
			final DefinedPermission[] permissions)
			throws AuthManagementException;

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
	 */
	public void revokePermissionsFromUser(final String username,
			final DefinedPermission[] permissions)
			throws AuthManagementException;

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
	 */
	public void grantPermissionsToRole(final String role,
			final DefinedPermission[] permissions)
			throws AuthManagementException;

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
	 */
	public void revokePermissionsFromRole(final String role,
			final DefinedPermission[] permissions)
			throws AuthManagementException;

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

	/**
	 * Gets all the users known to the manager.
	 * 
	 * @return all the users known to the manager
	 */
	public Set<String> getUsers();

	/**
	 * Gets all the roles known to the manager.
	 * 
	 * @return all the roles known to the manager
	 */
	public Set<String> getRoles();

	/**
	 * Get all the roles of the user.
	 * 
	 * @param username
	 *            the name of the user
	 * 
	 * @return the roles of the user
	 */
	public Set<String> getUserRoles(final String username);

	/**
	 * Gets all the permissions of the user. If the user has the permission to
	 * use a model specific permission for all models (i.e. because the user is
	 * an administrator), the {@code *} should be used as model identifier, to
	 * mark the permission to be available for all models.
	 * 
	 * @param username
	 *            the name of the user
	 * 
	 * @return the permissions of the user
	 */
	public Set<DefinedPermission> getUserPermissions(final String username);

	/**
	 * Gets the directly assigned user permission available to the specified
	 * user.
	 * 
	 * @param username
	 *            the name of the user
	 *            
	 * @return the permissions assigned to the user directly (i.e.
	 *         role-permissions are excluded)
	 */
	public Set<DefinedPermission> getAssignedUserPermissions(
			final String username);

	/**
	 * Gets all the permissions of the role. If the role has the permission to
	 * use a model specific permission for all models (i.e. because the user is
	 * an administrator), the {@code *} should be used as model identifier, to
	 * mark the permission to be available for all models.
	 * 
	 * @param role
	 *            the role
	 * 
	 * @return the permissions of the role
	 */
	public Set<DefinedPermission> getRolePermissions(final String role);

	/**
	 * Gets the name of the current user.
	 * 
	 * @return the name of the current user, must return {@code null} if no user
	 *         is currently authenticated
	 */
	public String getCurrentUsername();
}
