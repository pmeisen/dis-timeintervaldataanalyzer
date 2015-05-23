package net.meisen.dissertation.impl.parser.query.get;

import java.util.Collections;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

import net.meisen.dissertation.model.auth.IAuthManager;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.dissertation.model.data.FieldNameGenerator;
import net.meisen.dissertation.model.parser.query.IQueryResultSet;
import net.meisen.general.genmisc.types.Strings;

/**
 * Gets a query asking for a {@link GetResultType#ROLES}.
 * 
 * @author pmeisen
 * 
 */
public class GetResultRoles implements IQueryResultSet {
	private final IAuthManager authManager;

	/**
	 * Constructor specifying the {@code authManager} used.
	 * 
	 * @param authManager
	 *            the used {@code AuthManager}
	 * 
	 * @see IAuthManager
	 */
	public GetResultRoles(final IAuthManager authManager) {
		this.authManager = authManager;
	}

	@Override
	public int[] getCollectedIds() {
		return null;
	}

	@Override
	public Iterator<Object[]> iterator() {
		final Set<String> roles = authManager.getRoles();
		final Set<String> sortedRoles;
		if (roles == null) {
			sortedRoles = Collections.<String> emptySet();
		} else {
			sortedRoles = new TreeSet<String>();
			sortedRoles.addAll(roles);
		}

		return new Iterator<Object[]>() {
			private Iterator<String> it = sortedRoles.iterator();

			@Override
			public boolean hasNext() {
				return it.hasNext();
			}

			@Override
			public Object[] next() {
				final String role = it.next();
				final Set<DefinedPermission> perms = new TreeSet<DefinedPermission>(
						authManager.getRolePermissions(role));

				return new Object[] { role, Strings.join(",", perms) };
			}

			@Override
			public void remove() {
				throw new UnsupportedOperationException(
						"Remove is not supported.");
			}
		};
	}

	@Override
	public Class<?>[] getTypes() {
		return new Class<?>[] { String.class, String.class };
	}

	@Override
	public String[] getNames() {
		final FieldNameGenerator fng = FieldNameGenerator.get();
		return new String[] { fng.getRolesFieldName(),
				fng.getPermissionsFieldName() };
	}

}
