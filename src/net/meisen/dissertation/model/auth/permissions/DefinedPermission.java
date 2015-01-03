package net.meisen.dissertation.model.auth.permissions;

import java.util.regex.Pattern;

import net.meisen.dissertation.model.auth.IAuthManager;
import net.meisen.general.genmisc.types.Objects;

/**
 * A defined (i.e. if a model is needed, it's defined) {@code Permission}. Use
 * {@link Permission#create(String)} to create a {@code DefinedPermission}
 * instance.
 * 
 * @author pmeisen
 * 
 */
public class DefinedPermission implements Comparable<DefinedPermission> {
	/**
	 * Separator used if none is specified
	 */
	public final static String DEF_SEPARATOR = ".";

	private final Permission permission;
	private final String modelId;

	/**
	 * Use {@link Permission#create(String)} to create a
	 * {@code DefinedPermission} instance.
	 * 
	 * @param permission
	 *            the {@code Permission} of {@code this}
	 * @param modelId
	 *            the model belonging to the {@code Permission}, only allowed to
	 *            be unequal to {@code null} if a {@link Permission} with
	 *            {@link PermissionLevel#MODEL} is used
	 * 
	 * @throws IllegalArgumentException
	 *             if a {@code modelId} is defined for a {@code permission} with
	 *             a {@code PermissionLevel} uneqal to
	 *             {@link PermissionLevel#MODEL}
	 */
	protected DefinedPermission(final Permission permission,
			final String modelId) throws IllegalArgumentException {
		this.permission = permission;
		this.modelId = modelId;

		if (PermissionLevel.GLOBAL.equals(permission.getLevel())) {
			if (modelId != null) {
				throw new IllegalArgumentException(
						"A global-permission does not support the definition of a model-identifier.");
			}
		} else if (PermissionLevel.MODEL.equals(permission.getLevel())) {
			if (modelId == null) {
				throw new IllegalArgumentException(
						"A model-permission needs the definition of a model-identifier.");
			}
		} else {
			// nothing to be checked
		}
	}

	/**
	 * Gets the {@code Permission} of {@code this}.
	 * 
	 * @return the {@code Permission} of {@code this}
	 */
	public Permission getPermission() {
		return permission;
	}

	/**
	 * Gets the {@code modelId} of {@code this}. The method will return
	 * {@code null} if the level of the permission is global (i.e.
	 * {@link PermissionLevel#GLOBAL}).
	 * 
	 * @return the {@code modelId} of {@code this}
	 */
	public String getModelId() {
		return modelId;
	}

	@Override
	public String toString() {
		return toString(DEF_SEPARATOR);
	}

	/**
	 * A string representation which separates the different parts of the
	 * permission using the specified {@code separator}.
	 * 
	 * @param separator
	 *            the separator to separate the different parts of the
	 *            permission (i.e. level from model (if one is specified) from
	 *            permission)
	 * 
	 * @return the string representation
	 */
	public String toString(final String separator) {
		return permission.toString(modelId, separator);
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == this) {
			return true;
		} else if (obj instanceof DefinedPermission) {
			final DefinedPermission perm = (DefinedPermission) obj;

			return Objects.equals(getModelId(), perm.getModelId())
					&& Objects.equals(getPermission(), perm.getPermission());
		} else {
			return false;
		}
	}

	@Override
	public int hashCode() {
		return Objects.generateHashCode(7, 23, getPermission(), getModelId());
	}

	/**
	 * Check the specified set of permissions within the specified
	 * {@code AuthManager}.
	 * 
	 * @param authManager
	 *            the authentication manager used to verify the permissions
	 * @param permSets
	 *            the permissions to be verified
	 * 
	 * @return {@code true} if the authentication manager has the specified
	 *         permissions for the current subject
	 * 
	 * @see IAuthManager
	 */
	public static boolean checkPermission(final IAuthManager authManager,
			final DefinedPermission[][] permSets) {
		boolean permissionGranted = false;

		if (permSets == null) {
			permissionGranted = true;
		} else if (permSets.length == 0) {
			permissionGranted = true;
		} else {
			permissionGranted = false;

			// iterate and check
			for (final DefinedPermission[] permSet : permSets) {
				permissionGranted = true;

				// validate if the permission is really available
				for (final DefinedPermission perm : permSet) {
					if (!authManager.hasPermission(perm)) {
						permissionGranted = false;
						break;
					}
				}

				// if one of the sets grants permission to process stop
				if (permissionGranted) {
					break;
				}
			}
		}

		return permissionGranted;
	}

	/**
	 * Generates a string for the specified set of sets of
	 * {@code DefinedPermission}.
	 * 
	 * @param permSets
	 *            the set of sets
	 * 
	 * @return the string representation
	 */
	public static String toString(final DefinedPermission[][] permSets) {
		if (permSets == null || permSets.length == 0) {
			return "";
		}

		final StringBuilder sb = new StringBuilder();
		for (int k = 0; k < permSets.length; k++) {
			final DefinedPermission[] permSet = permSets[k];
			if (k > 0) {
				sb.append(", ");
			}

			// validate if the permission is really available
			sb.append("(");
			for (int i = 0; i < permSet.length; i++) {
				final DefinedPermission perm = permSet[i];
				if (i > 0) {
					sb.append(", ");
				}

				sb.append(perm.toString(DEF_SEPARATOR));
			}
			sb.append(")");
		}

		return sb.toString();
	}

	/**
	 * Retrieves {@code DefinedPermission} from a string representation using
	 * the {@link DefinedPermission#DEF_SEPARATOR}.
	 * 
	 * @param permission
	 *            the string representation of the {@code DefinedPermission}
	 * 
	 * @return the presented {@code DefinitionPermission}
	 * 
	 * @throws IllegalArgumentException
	 *             exception thrown if the permission cannot be resolved
	 */
	public static DefinedPermission fromString(final String permission)
			throws IllegalArgumentException {
		return fromString(permission, null);
	}

	/**
	 * Retrieves {@code DefinedPermission} from a string representation using
	 * the specified {@code separator}.
	 * 
	 * @param permission
	 *            the string representation of the {@code DefinedPermission}
	 * @param separator
	 *            the separator used by the representation
	 * 
	 * @return the presented {@code DefinitionPermission}
	 * 
	 * @throws IllegalArgumentException
	 *             exception thrown if the permission cannot be resolved
	 */
	public static DefinedPermission fromString(final String permission,
			final String separator) throws IllegalArgumentException {
		final String sep;
		if (separator == null) {
			sep = DEF_SEPARATOR;
		} else {
			sep = separator;
		}

		final String[] parts = permission.split("\\s*" + Pattern.quote(sep)
				+ "\\s*");
		final Permission perm;
		final String modelId;
		if (parts.length == 2) {
			PermissionLevel.valueOf(parts[0]);
			perm = Permission.valueOf(parts[1]);
			modelId = null;
		} else if (parts.length == 3) {
			PermissionLevel.valueOf(parts[0]);
			modelId = parts[1];
			perm = Permission.valueOf(parts[2]);
		} else {
			throw new IllegalArgumentException("The permission '" + permission
					+ "' cannot be resolved.");
		}

		return new DefinedPermission(perm, modelId);
	}

	@Override
	public int compareTo(final DefinedPermission o) {
		return toString().compareTo(o.toString());
	}
}
