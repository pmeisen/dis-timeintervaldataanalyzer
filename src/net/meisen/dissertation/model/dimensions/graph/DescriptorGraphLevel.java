package net.meisen.dissertation.model.dimensions.graph;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import net.meisen.dissertation.exceptions.DescriptorDimensionException;
import net.meisen.dissertation.model.dimensions.DescriptorLevel;
import net.meisen.dissertation.model.dimensions.DescriptorMember;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * A level within a {@code DescriptorGraph}. A level combines/groups nodes of
 * the graph. A level can thereby contain nodes of members of different
 * hierarchies.
 * 
 * @author pmeisen
 * 
 */
public class DescriptorGraphLevel {
	private final boolean shared;

	private final Map<String, DescriptorLevel> levels;

	private final Set<DescriptorGraphNode> nodes;
	private final Set<DescriptorGraphLevel> parents;
	private final Set<DescriptorGraphLevel> children;

	private Set<DescriptorGraphLevel> allParents;

	private int minDistanceToRoot = Integer.MIN_VALUE;
	private int maxDistanceToRoot = Integer.MIN_VALUE;

	/**
	 * Default constructor specifying if the level is shared.
	 * 
	 * @param shared
	 *            {@code true} if the level is shared, otherwise {@code false}
	 */
	public DescriptorGraphLevel(final boolean shared) {
		this.shared = shared;

		this.levels = new HashMap<String, DescriptorLevel>();
		this.nodes = new HashSet<DescriptorGraphNode>();
		this.parents = new HashSet<DescriptorGraphLevel>();
		this.children = new HashSet<DescriptorGraphLevel>();
	}

	/**
	 * Binds {@code this} to the specified {@code descLevel}. A level can be
	 * bound to several {@code DescriptorLevel} instances.
	 * 
	 * @param descLevel
	 *            the level to be bound to {@code this}
	 */
	public void bind(final DescriptorLevel descLevel) {

		if (!levels.isEmpty()) {
			final DescriptorLevel first = levels.values().iterator().next();
			if (!descLevel.getId().equals(first.getId())) {
				throw new ForwardedRuntimeException(
						DescriptorDimensionException.class, 1010,
						descLevel.getId(), first.getId());
			}
		}

		this.levels.put(descLevel.getHierachy().getId(), descLevel);
	}

	/**
	 * Adds a node to {@code this}.
	 * 
	 * @param node
	 *            the node to be added
	 */
	public void add(final DescriptorGraphNode node) {
		nodes.add(node);
	}

	/**
	 * Defines a parent-level of {@code this}.
	 * 
	 * @param parent
	 *            a parent level to be added
	 */
	public void addParent(final DescriptorGraphLevel parent) {

		// invalidate the all parents
		allParents = null;

		parents.add(parent);
	}

	/**
	 * Adds a child-level to {@code this}.
	 * 
	 * @param child
	 *            the child to be added
	 */
	public void addChild(final DescriptorGraphLevel child) {
		children.add(child);
	}

	/**
	 * Checks if the level is empty, i.e. has no nodes.
	 * 
	 * @return {@code true} if the level is empty, otherwise {@code false}
	 */
	public boolean isEmpty() {
		return nodes.isEmpty();
	}

	/**
	 * Gets all the nodes of {@code this}.
	 * 
	 * @return all the nodes of {@code this}
	 */
	public Set<DescriptorGraphNode> getNodes() {
		return Collections.unmodifiableSet(nodes);
	}

	/**
	 * Gets the parents of {@code this}.
	 * 
	 * @return the parents of {@code this}
	 */
	public Set<DescriptorGraphLevel> getParents() {
		return Collections.unmodifiableSet(parents);
	}

	/**
	 * Gets all the parent-levels, i.e. all the levels that are above
	 * {@code this} to the way to the root.
	 * 
	 * @return all the parents
	 */
	public Set<DescriptorGraphLevel> getAllParents() {

		if (allParents == null) {
			allParents = new HashSet<DescriptorGraphLevel>();

			if (!isRoot()) {
				allParents.addAll(parents);

				for (final DescriptorGraphLevel parent : parents) {
					allParents.addAll(parent.getAllParents());

					// make sure we don't have any cycle
					if (allParents.contains(this)) {
						throw new ForwardedRuntimeException(
								DescriptorDimensionException.class, 1018,
								getDescriptorLevelId());
					}
				}
			}
		}

		return allParents;
	}

	/**
	 * Gets the children-levels of {@code this}.
	 * 
	 * @return the children-levels of {@code this}
	 */
	public Set<DescriptorGraphLevel> getChildren() {
		return Collections.unmodifiableSet(children);
	}

	/**
	 * Checks if the level is a leaf-level (i.e. has no children).
	 * 
	 * @return {@code true} if the level is a leaf-level, otherwise
	 *         {@code false}
	 */
	public boolean isLeaf() {
		return children.isEmpty();
	}

	/**
	 * Checks if the level is a root-level (i.e. has no parents).
	 * 
	 * @return {@code true} if the level is a root-level, otherwise
	 *         {@code false}
	 */
	public boolean isRoot() {
		return parents.isEmpty();
	}

	/**
	 * Gets the minimal distance to the root from {@code this}.
	 * 
	 * @return the minimal distance to the root from {@code this}
	 */
	public int getMinDistance() {
		if (minDistanceToRoot == Integer.MIN_VALUE) {
			calcDistances();
		}

		return minDistanceToRoot;
	}

	/**
	 * Gets the maximal distance to the root from {@code this}.
	 * 
	 * @return the maximal distance to the root from {@code this}
	 */
	public int getMaxDistance() {
		if (maxDistanceToRoot == Integer.MIN_VALUE) {
			calcDistances();
		}

		return maxDistanceToRoot;
	}

	/**
	 * Calculates the distances to the root. A negative value is used if the
	 * root is not reachable.
	 */
	protected void calcDistances() {

		if (isEmpty()) {
			minDistanceToRoot = -1;
			maxDistanceToRoot = -1;
		} else {
			int minDistToRoot = -1;
			int maxDistToRoot = -1;
			for (final DescriptorGraphNode descriptorGraphNode : nodes) {
				final int tmpMinDistToRoot = descriptorGraphNode
						.getMinDistance();
				final int tmpMaxDistToRoot = descriptorGraphNode
						.getMaxDistance();

				if (tmpMinDistToRoot == -1 || tmpMaxDistToRoot == -1) {
					maxDistToRoot = -1;
					minDistToRoot = -1;
					break;
				}

				if (maxDistToRoot == -1 || tmpMaxDistToRoot > maxDistToRoot) {
					maxDistToRoot = tmpMaxDistToRoot;
				}
				if (minDistToRoot == -1 || tmpMinDistToRoot < minDistToRoot) {
					minDistToRoot = tmpMinDistToRoot;
				}
			}
			minDistanceToRoot = minDistToRoot;
			maxDistanceToRoot = maxDistToRoot;
		}
	}

	@Override
	public String toString() {
		return levels.toString() + " (" + nodes.toString() + ")";
	}

	/**
	 * Gets the {@code DescriptorLevel} for the specified hierarchy.
	 * 
	 * @param hierarchyId
	 *            the identifier of the hierarchy to get the
	 *            {@code DescriptorLevel} from
	 * 
	 * @return the {@code DescriptorLevel} found for the specified
	 *         {@code hierarchyId}, can be {@code null} if no level exists for
	 *         the hierarchy
	 */
	public DescriptorLevel getDescriptorLevel(final String hierarchyId) {
		return levels.get(hierarchyId);
	}

	/**
	 * Get the levels bound to {@code this}.
	 * 
	 * @return the levels bound to {@code this}
	 */
	public Collection<DescriptorLevel> getDescriptorLevels() {
		return Collections.unmodifiableCollection(levels.values());
	}

	/**
	 * Checks if the level is marked to be shared.
	 * 
	 * @return {@code true} if it is shared, otherwise {@code false}
	 */
	public boolean isShared() {
		return shared;
	}

	/**
	 * Get the identifier of the first {@code DescriptorLevel}. The first level
	 * is not specified, but often there is only one. Additionally, it is a good
	 * information needed when throwing exceptions.
	 * 
	 * @return the identifier of the first {@code DescriptorLevel}
	 */
	public String getDescriptorLevelId() {
		if (levels.isEmpty()) {
			return null;
		} else {
			final DescriptorLevel first = levels.values().iterator().next();
			return first.getId();
		}
	}

	/**
	 * Get the members of the level for the specified hierarchy.
	 * 
	 * @param hierarchyId
	 *            the identifier of the hierarchy
	 * 
	 * @return the members of {@code this} of the specified hierarchy
	 */
	public Set<DescriptorMember> getMembers(final String hierarchyId) {
		final Set<DescriptorMember> members = new HashSet<DescriptorMember>();
		if (hierarchyId == null) {
			return members;
		}

		for (final DescriptorGraphNode descriptorGraphNode : nodes) {
			final DescriptorMember member = descriptorGraphNode.getMember();

			// check the hierarchy
			if (hierarchyId.equals(member.getHierachy().getId())) {
				members.add(descriptorGraphNode.getMember());
			}
		}

		return members;
	}

	/**
	 * Gets all the leaf-members reachable from the specified member (by
	 * {@code memberId} and {@code hierarchyId}) of {@code this}.
	 * 
	 * @param hierarchyId
	 *            the identifier of the hierarchy the member of {@code this}
	 *            belongs to
	 * @param memberId
	 *            the identifier of {@code this} to start from
	 * 
	 * @return all the leaf-members reachable from the specified {@code member}
	 */
	public Set<DescriptorMember> getLeafMembers(final String hierarchyId,
			final String memberId) {
		if (hierarchyId == null) {
			return new HashSet<DescriptorMember>();
		}

		DescriptorGraphNode root = null;
		for (final DescriptorGraphNode descriptorGraphNode : nodes) {
			final DescriptorMember member = descriptorGraphNode.getMember();
			if (hierarchyId.equals(member.getHierachy().getId())
					&& memberId.equals(member.getId())) {
				root = descriptorGraphNode;
				break;
			}
		}

		// if we could not find the root, there are no leafs
		if (root == null) {
			return new HashSet<DescriptorMember>();
		} else {
			return root.getReachableLeafs(hierarchyId);
		}
	}
}
