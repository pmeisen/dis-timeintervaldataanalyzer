package net.meisen.dissertation.model.auth.permissions;

/**
 * A defined (i.e. if a model is needed, it's defined) {@code Permission}. Use
 * {@link Permission#create(String)} to create a {@code DefinedPermission}
 * instance.
 * 
 * @author pmeisen
 * 
 */
public class DefinedPermission {
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
	 * Gets the {@code modelId} of {@code this}.
	 * 
	 * @return the {@code modelId} of {@code this}
	 */
	public String getModelId() {
		return modelId;
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
}
