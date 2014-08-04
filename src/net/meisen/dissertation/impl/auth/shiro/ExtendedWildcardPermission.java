package net.meisen.dissertation.impl.auth.shiro;

import java.util.List;
import java.util.Set;

import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.general.genmisc.types.Strings;

import org.apache.shiro.authz.permission.WildcardPermission;

/**
 * An implementation of a {@code WildcardPermission} which is case-sensitive and
 * can retrieve the {@code DefinedPermission} defined by the {@code Permission}.
 * 
 * @author pmeisen
 * 
 */
public class ExtendedWildcardPermission extends WildcardPermission {
	private static final long serialVersionUID = -650246762519762847L;

	/**
	 * Default constructor.
	 * 
	 * @param perm
	 *            the permission or the expression of the permissions to be
	 *            represented by {@code this}.
	 */
	public ExtendedWildcardPermission(final String perm) {
		super(perm, true);
	}

	/**
	 * Gets the {@code DefinedPermission} of {@code this}. The method returns
	 * {@code null} if
	 * 
	 * @return the {@code DefinedPermission} represented by {@code this}, or
	 *         {@code null} if wild-characters are used
	 */
	public DefinedPermission get() {
		final List<Set<String>> parts = getParts();

		if (parts.size() == 3) {
			final String level = flat(parts.get(0));
			final String model = flat(parts.get(1));
			final String permission = flat(parts.get(2));

			if (level == null || model == null || permission == null) {
				return null;
			}

			final String strDef = Strings.join(DefinedPermission.DEF_SEPARATOR,
					level, model, permission);
			final DefinedPermission def = DefinedPermission.fromString(strDef);

			return def;
		} else if (parts.size() == 2) {
			final String level = flat(parts.get(0));
			final String permission = flat(parts.get(1));

			if (level == null || permission == null) {
				return null;
			}

			final String strDef = Strings.join(DefinedPermission.DEF_SEPARATOR,
					level, permission);
			final DefinedPermission def = DefinedPermission.fromString(strDef);

			return def;
		} else {
			return null;
		}
	}

	/**
	 * Flattens the different sub-parts of the set.
	 * 
	 * @param set
	 *            the set to be flattened
	 * 
	 * @return the flattened string or {@code null} if one of the sub-parts
	 *         contained a wild-character
	 */
	protected String flat(final Set<String> set) {
		final StringBuilder sb = new StringBuilder();
		for (final String s : set) {

			// if we find a wildcard the flatting is invalid
			if (s.contains(WILDCARD_TOKEN)) {
				return null;
			}

			sb.append(s);
		}

		return sb.toString();
	}
}