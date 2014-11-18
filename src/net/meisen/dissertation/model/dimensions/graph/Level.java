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

public class Level {
	private final boolean shared;

	private final Map<String, DescriptorLevel> levels;

	private final Set<Node> nodes;
	private final Set<Level> parents;
	private final Set<Level> children;

	private Set<Level> allParents;

	private int minDistanceToRoot = Integer.MIN_VALUE;
	private int maxDistanceToRoot = Integer.MIN_VALUE;

	public Level(final boolean shared) {
		this.shared = shared;

		this.levels = new HashMap<String, DescriptorLevel>();
		this.nodes = new HashSet<Node>();
		this.parents = new HashSet<Level>();
		this.children = new HashSet<Level>();
	}

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

	public void add(final Node node) {
		nodes.add(node);
	}

	public void addParent(final Level parent) {

		// invalidate the all parents
		allParents = null;

		parents.add(parent);
	}

	public void addChild(final Level child) {
		children.add(child);
	}

	public boolean isEmpty() {
		return nodes.isEmpty();
	}

	public Set<Node> getNodes() {
		return Collections.unmodifiableSet(nodes);
	}

	public Set<Level> getParents() {
		return Collections.unmodifiableSet(parents);
	}

	public Set<Level> getAllParents() {

		if (allParents == null) {
			allParents = new HashSet<Level>();

			if (!isRoot()) {
				allParents.addAll(parents);

				for (final Level parent : parents) {
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

	public Set<Level> getChildren() {
		return Collections.unmodifiableSet(children);
	}

	public boolean isLeaf() {
		return children.isEmpty();
	}

	public boolean isRoot() {
		return parents.isEmpty();
	}

	public int getMinDistance() {
		if (minDistanceToRoot == Integer.MIN_VALUE) {
			calcDistances();
		}

		return minDistanceToRoot;
	}

	public int getMaxDistance() {
		if (maxDistanceToRoot == Integer.MIN_VALUE) {
			calcDistances();
		}

		return maxDistanceToRoot;
	}

	protected void calcDistances() {

		if (isEmpty()) {
			minDistanceToRoot = -1;
			maxDistanceToRoot = -1;
		} else {
			int minDistToRoot = -1;
			int maxDistToRoot = -1;
			for (final Node node : nodes) {
				final int tmpMinDistToRoot = node.getMinDistance();
				final int tmpMaxDistToRoot = node.getMaxDistance();

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

	public DescriptorLevel getDescriptorLevel(final String hierarchyId) {
		return levels.get(hierarchyId);
	}

	public Collection<DescriptorLevel> getDescriptorLevels() {
		return Collections.unmodifiableCollection(levels.values());
	}

	public boolean isShared() {
		return shared;
	}

	public String getDescriptorLevelId() {
		if (levels.isEmpty()) {
			return null;
		} else {
			final DescriptorLevel first = levels.values().iterator().next();
			return first.getId();
		}
	}

	public Set<DescriptorMember> getMembers(final String hierarchyId) {
		final Set<DescriptorMember> members = new HashSet<DescriptorMember>();
		if (hierarchyId == null) {
			return members;
		}

		for (final Node node : nodes) {
			final DescriptorMember member = node.getMember();

			// check the hierarchy
			if (hierarchyId.equals(member.getHierachy().getId())) {
				members.add(node.getMember());
			}
		}

		return members;
	}

	public Set<DescriptorMember> getLeafMembers(final String hierarchyId,
			final String memberId) {
		if (hierarchyId == null) {
			return new HashSet<DescriptorMember>();
		}

		Node root = null;
		for (final Node node : nodes) {
			final DescriptorMember member = node.getMember();
			if (hierarchyId.equals(member.getHierachy().getId())
					&& memberId.equals(member.getId())) {
				root = node;
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
