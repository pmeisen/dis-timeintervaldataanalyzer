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
 * Gets a query asking for a {@link GetResultType#USERS}.
 * 
 * @author pmeisen
 * 
 */
public class GetResultUsers implements IQueryResultSet {
	private final IAuthManager authManager;

	/**
	 * Constructor specifying the {@code authManager} used.
	 * 
	 * @param authManager
	 *            the used {@code AuthManager}
	 * 
	 * @see IAuthManager
	 */
	public GetResultUsers(final IAuthManager authManager) {
		this.authManager = authManager;
	}

	@Override
	public int[] getCollectedIds() {
		return null;
	}

	@Override
	public Iterator<Object[]> iterator() {
		final Set<String> users = authManager.getUsers();
		final Set<String> sortedUsers;
		if (users == null) {
			sortedUsers = Collections.<String> emptySet();
		} else {
			sortedUsers = new TreeSet<String>();
			sortedUsers.addAll(users);
		}

		return new Iterator<Object[]>() {
			private Iterator<String> it = sortedUsers.iterator();

			@Override
			public boolean hasNext() {
				return it.hasNext();
			}

			@Override
			public Object[] next() {
				final String user = it.next();
				final Set<String> roles = new TreeSet<String>(
						authManager.getUserRoles(user));
				final Set<DefinedPermission> perms = new TreeSet<DefinedPermission>(
						authManager.getUserPermissions(user));

				return new Object[] { user, Strings.join(",", roles),
						Strings.join(",", perms) };
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
		return new Class<?>[] { String.class, String.class, String.class };
	}

	@Override
	public String[] getNames() {
		final FieldNameGenerator fng = FieldNameGenerator.get();
		return new String[] { fng.getUserFieldName(), fng.getRolesFieldName(),
				fng.getPermissionsFieldName() };
	}

}
