package net.meisen.dissertation.model.dimensions;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

import net.meisen.dissertation.exceptions.DescriptorDimensionException;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Objects;

/**
 * A hierarchy defined for a {@code DescirptorModel} or more specific for a
 * {@code DescriptorDimension}.
 * 
 * @author pmeisen
 * 
 */
public class DescriptorHierarchy {

	private final String id;
	private final String name;

	private final DescriptorDimension descriptorDimension;
	private final DescriptorMemberManager memberManager;
	private final Map<String, DescriptorLevel> levels;

	/**
	 * Default constructor of a {@code DescriptorHierarchy} specifying the
	 * dimension, the id and the name of the hierarchy.
	 * 
	 * @param descriptorDimension
	 *            the dimension the hierarchy belongs to, cannot be {@code null}
	 * @param id
	 *            the identifier of the hierarchy, cannot be {@code null}
	 * @param name
	 *            the name of the hierarchy, can be {@code null}
	 */
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
				rootLevel.getName(), null, false, rootLevel, null);
	}

	/**
	 * Creates and adds the specified member to the hierarchy. The member
	 * created is a leaf-member, which is associated to a specific descriptor.
	 * 
	 * @param descriptorId
	 *            the descriptor the member is associated with
	 * @param levelId
	 *            the level the member belongs to
	 * @param rollUpTo
	 *            a list of member-identifiers the member can roll up tp
	 * 
	 * @return the created {@code DescriptorMember}
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the member could not be created, e.g. because the
	 *             identifier is used by another member
	 * 
	 * @see DescriptorMember
	 */
	public DescriptorMember addDescriptorMember(final String descriptorId,
			final String levelId, final Collection<String> rollUpTo)
			throws ForwardedRuntimeException {
		return addDescriptorMember(descriptorId, descriptorId, levelId,
				rollUpTo);
	}

	/**
	 * Creates and adds the specified member to the hierarchy. The member
	 * created is a leaf-member, which is associated to a specific descriptor.
	 * 
	 * @param id
	 *            the identifier of the member
	 * @param descriptor
	 *            the descriptor the member is associated with
	 * @param levelId
	 *            the level the member belongs to
	 * @param rollUpTo
	 *            a list of member-identifiers the member can roll up tp
	 * 
	 * @return the created {@code DescriptorMember}
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the member could not be created, e.g. because the
	 *             identifier is used by another member
	 * 
	 * @see DescriptorMember
	 */
	public DescriptorMember addDescriptorMember(final String id,
			final String descriptor, final String levelId,
			final Collection<String> rollUpTo) throws ForwardedRuntimeException {
		return addDescriptorMember(id, descriptor, descriptor, levelId,
				rollUpTo);
	}

	/**
	 * Creates and adds the specified member to the hierarchy. The member
	 * created is a leaf-member, which is associated to a specific descriptor.
	 * 
	 * @param id
	 *            the identifier of the member
	 * @param name
	 *            the name of the member
	 * @param descriptor
	 *            the descriptor the member is associated with
	 * @param levelId
	 *            the level the member belongs to
	 * @param rollUpTo
	 *            a list of member-identifiers the member can roll up tp
	 * 
	 * @return the created {@code DescriptorMember}
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the member could not be created, e.g. because the
	 *             identifier is used by another member
	 * 
	 * @see DescriptorMember
	 */
	public DescriptorMember addDescriptorMember(final String id,
			final String name, final String descriptor, final String levelId,
			final Collection<String> rollUpTo) throws ForwardedRuntimeException {
		return addPatternMember(id, name, Pattern.quote(descriptor), false,
				levelId, rollUpTo);
	}

	/**
	 * Creates and adds the specified member to the hierarchy. The member
	 * created is a leaf-member, which is associated to specific descriptors by
	 * an regular expression.
	 * 
	 * @param id
	 *            the identifier of the member
	 * @param pattern
	 *            the pattern specifying the descriptors the member is
	 *            associated with
	 * @param includeNull
	 *            {@code true} if the member also contains {@code null}
	 *            descriptors, otherwise {@code false}
	 * @param levelId
	 *            the level the member belongs to
	 * @param rollUpTo
	 *            a list of member-identifiers the member can roll up tp
	 * 
	 * @return the created {@code DescriptorMember}
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the member could not be created, e.g. because the
	 *             identifier is used by another member
	 * 
	 * @see DescriptorMember
	 */
	public DescriptorMember addPatternMember(final String id,
			final String pattern, final boolean includeNull,
			final String levelId, final Collection<String> rollUpTo)
			throws ForwardedRuntimeException {
		return addPatternMember(id, id, pattern, includeNull, levelId, rollUpTo);
	}

	/**
	 * Creates and adds the specified member to the hierarchy. The member
	 * created is a leaf-member, which is associated to specific descriptors by
	 * an regular expression.
	 * 
	 * @param id
	 *            the identifier of the member
	 * @param name
	 *            the name of the member
	 * @param pattern
	 *            the pattern specifying the descriptors the member is
	 *            associated with
	 * @param includeNull
	 *            {@code true} if the member also contains {@code null}
	 *            descriptors, otherwise {@code false}
	 * @param levelId
	 *            the level the member belongs to
	 * @param rollUpTo
	 *            a list of member-identifiers the member can roll up tp
	 * 
	 * @return the created {@code DescriptorMember}
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the member could not be created, e.g. because the
	 *             identifier is used by another member
	 * 
	 * @see DescriptorMember
	 */
	public DescriptorMember addPatternMember(final String id,
			final String name, final String pattern, final boolean includeNull,
			final String levelId, final Collection<String> rollUpTo)
			throws ForwardedRuntimeException {
		final DescriptorLevel level = resolveMemberLevel(levelId);
		return memberManager.addMember(id, name, pattern, includeNull, level,
				rollUpTo);
	}

	/**
	 * Creates and adds the specified member to the hierarchy. The member
	 * created is a non-leaf-member.
	 * 
	 * @param id
	 *            the identifier of the member
	 * @param levelId
	 *            the level the member belongs to
	 * @param rollUpTo
	 *            a list of member-identifiers the member can roll up tp
	 * 
	 * @return the created {@code DescriptorMember}
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the member could not be created, e.g. because the
	 *             identifier is used by another member
	 * 
	 * @see DescriptorMember
	 */
	public DescriptorMember addMember(final String id, final String levelId,
			final Collection<String> rollUpTo) throws ForwardedRuntimeException {
		return addMember(id, id, levelId, rollUpTo);
	}

	/**
	 * Creates and adds the specified member to the hierarchy. The member
	 * created is a non-leaf-member.
	 * 
	 * @param id
	 *            the identifier of the member
	 * @param name
	 *            the name of the member
	 * @param levelId
	 *            the level the member belongs to
	 * @param rollUpTo
	 *            a list of member-identifiers the member can roll up tp
	 * 
	 * @return the created {@code DescriptorMember}
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the member could not be created, e.g. because the
	 *             identifier is used by another member
	 * 
	 * @see DescriptorMember
	 */
	public DescriptorMember addMember(final String id, final String name,
			final String levelId, final Collection<String> rollUpTo)
			throws ForwardedRuntimeException {
		final DescriptorLevel level = resolveMemberLevel(levelId);
		return memberManager.addMember(id, name, null, false, level, rollUpTo);
	}

	/**
	 * Modifies a level, i.e. it's name. The method can also modify the name of
	 * the root-level. If the root-level is modified the name of the one and
	 * only root-member is modified as well.
	 * 
	 * @param levelId
	 *            the identifier of the level to be modified
	 * @param name
	 *            the new name of the level
	 */
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

	/**
	 * Helper method to resolve a {@code levelId}. The method is used to
	 * retrieve a specific level (which is not root-level). Retrieving means
	 * that the level is created if it does not exist.
	 * 
	 * @param levelId
	 *            the identifier of the level
	 * 
	 * @return the newly created or already available level associated to the
	 *         specified {@code levelId}
	 */
	protected DescriptorLevel resolveMemberLevel(final String levelId) {

		// make sure that the member is not assigned to the root
		if (DescriptorDimension.ROOT_LEVEL_ID.equals(levelId)) {
			throw new ForwardedRuntimeException(
					DescriptorDimensionException.class, 1003);
		}

		return resolveLevel(levelId);
	}

	/**
	 * Helper method to resolve a {@code levelId}. The method is used to
	 * retrieve a specific level (including the root-level). Retrieving means
	 * that the level is created if it does not exist.
	 * 
	 * @param levelId
	 *            the identifier of the level
	 * 
	 * @return the newly created or already available level associated to the
	 *         specified {@code levelId}
	 */
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
		return getName() + " (" + getId() + ")";
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

	/**
	 * Gets the identifier of {@code this}.
	 * 
	 * @return the identifier of {@code this}
	 */
	public String getId() {
		return id;
	}

	/**
	 * Gets the name of {@code this}.
	 * 
	 * @return the name of {@code this}
	 */
	public String getName() {
		return name == null ? id : name;
	}

	/**
	 * Gets the levels of {@code this}.
	 * 
	 * @return the levels of {@code this}
	 */
	public Collection<DescriptorLevel> getLevels() {
		return Collections.unmodifiableCollection(levels.values());
	}

	/**
	 * Gets all the members of {@code this}.
	 * 
	 * @return all the members of {@code this}
	 */
	public Collection<DescriptorMember> getMembers() {
		return memberManager.getMembers();
	}

	/**
	 * Gets all the members of the specified level.
	 * 
	 * @param levelId
	 *            the identifier of the level to get the members for
	 * 
	 * @return the members of {@code this} associated to the specified level
	 */
	public Collection<DescriptorMember> getMembers(final String levelId) {
		return memberManager.getMembers(levelId);
	}
}
