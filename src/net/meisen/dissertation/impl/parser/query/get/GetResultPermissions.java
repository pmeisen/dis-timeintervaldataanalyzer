package net.meisen.dissertation.impl.parser.query.get;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import net.meisen.dissertation.model.auth.IAuthManager;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.dissertation.model.data.FieldNameGenerator;
import net.meisen.dissertation.model.parser.query.IQueryResultSet;

/**
 * The result of the get-query to retrieve information about the different
 * permissions.
 * 
 * @author pmeisen
 * 
 */
public class GetResultPermissions implements IQueryResultSet {
	private final IAuthManager authManager;
	private final Set<String> tidaModels;

	/**
	 * Constructor which creates a result with no models.
	 * 
	 * @param authManager
	 *            the used {@code AuthManager}
	 * 
	 * @see IAuthManager
	 */
	public GetResultPermissions(final IAuthManager authManager) {
		this(authManager, null);
	}

	/**
	 * Constructor which specifies the {@code authManager} and the
	 * {@code tidaModels} of the result.
	 * 
	 * @param authManager
	 *            the used {@code AuthManager}
	 * @param tidaModels
	 *            the result, i.e. the models
	 * 
	 * @see IAuthManager
	 */
	public GetResultPermissions(final IAuthManager authManager,
			final Set<String> tidaModels) {
		this.authManager = authManager;

		if (tidaModels == null) {
			this.tidaModels = new HashSet<String>();
		} else {
			this.tidaModels = tidaModels;
		}
	}

	@Override
	public int[] getCollectedIds() {
		return null;
	}

	@Override
	public Iterator<Object[]> iterator() {
		final List<Object[]> data = new ArrayList<Object[]>();

		// get the users
		final Set<String> users = authManager.getUsers();
		for (final String user : users) {
			final Set<DefinedPermission> perms = new TreeSet<DefinedPermission>(
					authManager.getUserPermissions(user));

			for (final DefinedPermission perm : perms) {
				final String permModelId = perm.getModelId();
				if ("*".equals(permModelId)) {
					data.add(new Object[] { user, permModelId, perm.toString() });

					// all models are available
					for (final String modelId : tidaModels) {
						data.add(new Object[] {
								user,
								modelId,
								perm.getPermission().toString(modelId,
										DefinedPermission.DEF_SEPARATOR) });
					}
				} else {
					data.add(new Object[] { user, perm.getModelId(),
							perm.toString() });
				}
			}
		}

		return data.iterator();
	}

	@Override
	public Class<?>[] getTypes() {
		return new Class<?>[] { String.class, String.class, String.class };
	}

	@Override
	public String[] getNames() {
		return new String[] { FieldNameGenerator.get().getUserFieldName(),
				FieldNameGenerator.get().getModelIdFieldName(),
				FieldNameGenerator.get().getPermissionFieldName() };
	}

	/**
	 * Get the amount of models within the result.
	 * 
	 * @return the amount of models
	 */
	public int size() {
		return tidaModels.size();
	}

}
