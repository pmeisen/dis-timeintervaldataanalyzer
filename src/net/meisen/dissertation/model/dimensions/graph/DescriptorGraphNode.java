package net.meisen.dissertation.model.dimensions.graph;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import net.meisen.dissertation.exceptions.DescriptorDimensionException;
import net.meisen.dissertation.model.dimensions.DescriptorDimension;
import net.meisen.dissertation.model.dimensions.DescriptorLevel;
import net.meisen.dissertation.model.dimensions.DescriptorMember;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Objects;

/**
 * A node within the {@code DescriptorGraph} representing a
 * {@code DescriptorMember}.
 * 
 * @author pmeisen
 * 
 */
public class DescriptorGraphNode {

	private final DescriptorMember member;

	private final Set<DescriptorGraphNode> parents;
	private final Set<DescriptorGraphNode> children;

	private int minDistanceToRoot = Integer.MIN_VALUE;
	private int maxDistanceToRoot = Integer.MIN_VALUE;

	/**
	 * The default constructor specifying the {@code member} the
	 * {@code DescriptorGraphNode} belongs to.
	 * 
	 * @param member
	 *            the {@code member} the {@code DescriptorGraphNode} belongs to
	 */
	public DescriptorGraphNode(final DescriptorMember member) {
		this.member = member;

		this.parents = new HashSet<DescriptorGraphNode>();
		this.children = new HashSet<DescriptorGraphNode>();
	}

	/**
	 * Gets or determines the minimal distance to root.
	 * 
	 * @return the minimal distance to the root of the graph, a negative value
	 *         (i.e. -1) is returned if there is no path to the root
	 */
	public int getMinDistance() {
		if (minDistanceToRoot == Integer.MIN_VALUE) {
			calcDistances(null);
		}

		return minDistanceToRoot;
	}

	/**
	 * Gets or determines the maximal distance to root.
	 * 
	 * @return the maximal distance to the root of the graph, a negative value
	 *         (i.e. -1) is returned if there is no path to the root
	 */
	public int getMaxDistance() {
		if (maxDistanceToRoot == Integer.MIN_VALUE) {
			calcDistances(null);
		}

		return maxDistanceToRoot;
	}

	/**
	 * Calculate the min- and max-distances between {@code this} and the root.
	 * 
	 * @param visited
	 *            internally used when recursively calculating the distance,
	 *            should normally called with {@code null}
	 */
	protected void calcDistances(Set<DescriptorGraphNode> visited) {

		// make sure calculation is needed
		if (minDistanceToRoot != Integer.MIN_VALUE
				&& maxDistanceToRoot != Integer.MIN_VALUE) {
			return;
		}

		if (DescriptorDimension.ROOT_MEMBER_ID.equals(member.getId())) {
			if (!isSink()) {
				throw new ForwardedRuntimeException(
						DescriptorDimensionException.class, 1015, getParents()
								.toString());
			}

			minDistanceToRoot = 0;
			maxDistanceToRoot = 0;
		} else {
			if (visited == null) {
				visited = new HashSet<DescriptorGraphNode>();
			}

			// add this one as visited node
			if (!visited.add(this)) {
				throw new ForwardedRuntimeException(
						DescriptorDimensionException.class, 1014);
			}

			int minDistToRoot = -1;
			int maxDistToRoot = -1;
			for (final DescriptorGraphNode parent : parents) {

				// make sure everything is calculated with the current path
				parent.calcDistances(visited);

				// check if we found a new min-value
				final int tmpMinDistToRoot = parent.getMinDistance();
				if (minDistToRoot == -1 || tmpMinDistToRoot < minDistToRoot) {
					minDistToRoot = tmpMinDistToRoot;
				}

				// check if we found a new max-value
				final int tmpMaxDistToRoot = parent.getMaxDistance();
				if (maxDistToRoot == -1 || tmpMaxDistToRoot > maxDistToRoot) {
					maxDistToRoot = tmpMaxDistToRoot;
				}
			}

			// add one for the current node
			minDistanceToRoot = minDistToRoot + 1;
			maxDistanceToRoot = maxDistToRoot + 1;
		}
	}

	/**
	 * Adds a parent to {@code this}.
	 * 
	 * @param parent
	 *            the parent to be added
	 */
	public void addParent(final DescriptorGraphNode parent) {
		parents.add(parent);
	}

	/**
	 * Adds a child to {@code this}.
	 * 
	 * @param child
	 *            the child to be added
	 */
	public void addChild(final DescriptorGraphNode child) {
		children.add(child);
	}

	/**
	 * Checks if the node is a source, i.e. has no children.
	 * 
	 * @return {@code true} if the node is a source, otherwise {@code false}
	 */
	public boolean isSource() {
		return children.isEmpty();
	}

	/**
	 * Checks if the node is a sink, i.e. is a root (has no parents).
	 * 
	 * @return {@code true} if the node is a root, otherwise {@code false}
	 */
	public boolean isSink() {
		return parents.isEmpty();
	}

	/**
	 * Gets all the leafs reachable from {@code this}. The {@code hierarchyId}
	 * must be specified to ensure the correct finding across shared levels.
	 * 
	 * @param hierarchyId
	 *            the identifier of the hierarchy to retrieve the leaf-members
	 *            for
	 * 
	 * @return all the leaf-members reachable from {@code this}
	 */
	public Set<DescriptorMember> getReachableLeafs(final String hierarchyId) {
		final Set<DescriptorMember> members = new HashSet<DescriptorMember>();

		if (hierarchyId == null) {
			// nothing to do
		} else if (this.isSource()) {
			final DescriptorMember member = getMember();
			if (hierarchyId.equals(member.getHierachy().getId())) {
				members.add(member);
			}
		} else {
			for (final DescriptorGraphNode descriptorGraphNode : getChildren()) {
				members.addAll(descriptorGraphNode
						.getReachableLeafs(hierarchyId));
			}
		}

		return members;
	}

	@Override
	public int hashCode() {
		return member.getId().hashCode();
	}

	@Override
	public boolean equals(final Object obj) {

		if (obj == this) {
			return true;
		} else if (obj == null) {
			return false;
		} else if (obj instanceof DescriptorGraphNode) {
			final DescriptorGraphNode cmpNode = (DescriptorGraphNode) obj;
			return Objects.equals(member, cmpNode.member);
		} else {
			return false;
		}
	}

	/**
	 * Gets the children of {@code this}.
	 * 
	 * @return the children of {@code this}
	 */
	public Set<DescriptorGraphNode> getChildren() {
		return Collections.unmodifiableSet(children);
	}

	/**
	 * Gets the parents of {@code this}.
	 * 
	 * @return the parents of {@code this}.
	 */
	public Set<DescriptorGraphNode> getParents() {
		return Collections.unmodifiableSet(parents);
	}

	/**
	 * Gets the member {@code this} belongs to.
	 * 
	 * @return the member {@code this} belongs to
	 */
	public DescriptorMember getMember() {
		return member;
	}

	/**
	 * Gets the level {@code this} belongs to.
	 * 
	 * @return the level {@code this} belongs to
	 */
	public DescriptorLevel getLevel() {
		return member.getLevel();
	}

	@Override
	public String toString() {
		return member.toString() + " " + member.getRollUpTo().toString();
	}

	/**
	 * Checks if the specified {@code node} is reachable from {@code this}.
	 * 
	 * @param node
	 *            the node to be checked for reachability
	 * 
	 * @return {@code true} if the node is reachable, otherwise {@code false}
	 */
	public boolean canReach(final DescriptorGraphNode node) {

		// the node can reach itself
		if (equals(node)) {
			return true;
		}
		// make sure there are no cycles
		else {
			calcDistances(null);
		}

		// check if it can reach any parent
		for (final DescriptorGraphNode parent : parents) {
			if (parent.canReach(node)) {
				return true;
			}
		}

		return false;
	}
}
