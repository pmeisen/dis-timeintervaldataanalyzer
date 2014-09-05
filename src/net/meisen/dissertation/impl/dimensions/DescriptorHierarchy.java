package net.meisen.dissertation.impl.dimensions;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

import net.meisen.dissertation.exceptions.DescriptorDimensionException;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Objects;

public class DescriptorHierarchy {

	private final String id;
	private final String name;

	private final DescriptorDimension descriptorDimension;
	private final DescriptorMemberManager memberManager;
	private final Map<String, DescriptorLevel> levels;

	public DescriptorHierarchy(final DescriptorDimension descriptorDimension,
			final String id, final String name) {
		this.id = id;
		this.name = name;

		this.descriptorDimension = descriptorDimension;
		this.memberManager = new DescriptorMemberManager(this);
		this.levels = new HashMap<String, DescriptorLevel>();

		// add the root
		final DescriptorLevel rootLevel = new DescriptorLevel(
				DescriptorDimension.ROOT_LEVEL_ID, this);
		this.levels.put(DescriptorDimension.ROOT_LEVEL_ID, rootLevel);
		memberManager.addMember(DescriptorDimension.ROOT_MEMBER_ID,
				rootLevel.getName(), null, rootLevel, null);
	}

	public DescriptorMember addDescriptorMember(final String descriptor,
			final String levelId, final Collection<String> rollUpTo) {
		return addDescriptorMember(descriptor, descriptor, levelId, rollUpTo);
	}

	public DescriptorMember addDescriptorMember(final String id,
			final String descriptor, final String levelId,
			final Collection<String> rollUpTo) {
		return addDescriptorMember(id, descriptor, descriptor, levelId,
				rollUpTo);
	}

	public DescriptorMember addDescriptorMember(final String id,
			final String name, final String descriptor, final String levelId,
			final Collection<String> rollUpTo) {
		return addPatternMember(id, name, Pattern.quote(descriptor), levelId,
				rollUpTo);
	}

	public DescriptorMember addPatternMember(final String id,
			final String pattern, final String levelId,
			final Collection<String> rollUpTo) {
		return addPatternMember(id, id, pattern, levelId, rollUpTo);
	}

	public DescriptorMember addPatternMember(final String id,
			final String name, final String pattern, final String levelId,
			final Collection<String> rollUpTo) {
		final DescriptorLevel level = resolveMemberLevel(levelId);
		return memberManager.addMember(id, name, pattern, level, rollUpTo);
	}

	public DescriptorMember addMember(final String id, final String levelId,
			final Collection<String> rollUpTo) {
		return addMember(id, id, levelId, rollUpTo);
	}

	public DescriptorMember addMember(final String id, final String name,
			final String levelId, final Collection<String> rollUpTo) {
		final DescriptorLevel level = resolveMemberLevel(levelId);
		return memberManager.addMember(id, name, null, level, rollUpTo);
	}

	public void modifyLevel(final String levelId, final String name) {
		final DescriptorLevel level = resolveLevel(levelId);
		level.setName(name);

		// we also rename the member if the root level is renamed
		if (DescriptorDimension.ROOT_LEVEL_ID.equals(levelId)) {
			final DescriptorMember rootMember = memberManager
					.getMember(DescriptorDimension.ROOT_MEMBER_ID);
			if (rootMember != null) {
				rootMember.setName(name);
			}
		}
	}

	protected DescriptorLevel resolveMemberLevel(final String levelId) {

		// make sure that the member is not assigned to the root
		if (DescriptorDimension.ROOT_LEVEL_ID.equals(levelId)) {
			throw new ForwardedRuntimeException(
					DescriptorDimensionException.class, 1003);
		}

		return resolveLevel(levelId);
	}

	protected DescriptorLevel resolveLevel(final String levelId) {
		DescriptorLevel level = levels.get(levelId);

		// add the level
		if (level == null) {
			if (DescriptorDimension.ROOT_LEVEL_ID.equals(levelId)) {
				throw new ForwardedRuntimeException(
						DescriptorDimensionException.class, 1008);
			}

			level = new DescriptorLevel(levelId, this);
			levels.put(levelId, level);
		}

		return level;
	}

	@Override
	public String toString() {
		return getName() + "(" + getId() + ")";
	}

	@Override
	public boolean equals(final Object obj) {

		if (obj == this) {
			return true;
		} else if (obj == null) {
			return false;
		} else if (obj instanceof DescriptorHierarchy) {
			final DescriptorHierarchy dh = (DescriptorHierarchy) obj;
			return Objects.equals(getId(), dh.getId())
					&& Objects.equals(descriptorDimension,
							dh.descriptorDimension);
		} else {
			return false;
		}
	}

	@Override
	public int hashCode() {
		return getId().hashCode();
	}

	public String getId() {
		return id;
	}

	public String getName() {
		return name;
	}

	public Collection<DescriptorLevel> getLevels() {
		return Collections.unmodifiableCollection(levels.values());
	}

	public Collection<DescriptorMember> getMembers() {
		return memberManager.getMembers();
	}
}
