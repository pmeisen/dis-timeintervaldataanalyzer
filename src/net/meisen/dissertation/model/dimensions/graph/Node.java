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

public class Node {

	private final DescriptorMember member;

	private final Set<Node> parents;
	private final Set<Node> children;

	private int minDistanceToRoot = Integer.MIN_VALUE;
	private int maxDistanceToRoot = Integer.MIN_VALUE;

	public Node(final DescriptorMember member) {
		this.member = member;

		this.parents = new HashSet<Node>();
		this.children = new HashSet<Node>();
	}

	public int getMinDistance() {
		if (minDistanceToRoot == Integer.MIN_VALUE) {
			calcDistances(null);
		}

		return minDistanceToRoot;
	}

	public int getMaxDistance() {
		if (maxDistanceToRoot == Integer.MIN_VALUE) {
			calcDistances(null);
		}

		return maxDistanceToRoot;
	}

	protected void calcDistances(Set<Node> visited) {

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
				visited = new HashSet<Node>();
			}

			// add this one as visited node
			if (!visited.add(this)) {
				throw new ForwardedRuntimeException(
						DescriptorDimensionException.class, 1014);
			}

			int minDistToRoot = -1;
			int maxDistToRoot = -1;
			for (final Node parent : parents) {

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

	public void addParent(final Node parent) {
		parents.add(parent);
	}

	public void addChild(final Node child) {
		children.add(child);
	}

	public boolean isSource() {
		return children.isEmpty();
	}

	public boolean isSink() {
		return parents.isEmpty();
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
		} else if (obj instanceof Node) {
			final Node cmpNode = (Node) obj;
			return Objects.equals(member, cmpNode.member);
		} else {
			return false;
		}
	}

	public Set<Node> getChildren() {
		return Collections.unmodifiableSet(children);
	}

	public Set<Node> getParents() {
		return Collections.unmodifiableSet(parents);
	}

	public DescriptorMember getMember() {
		return member;
	}

	public DescriptorLevel getLevel() {
		return member.getLevel();
	}

	@Override
	public String toString() {
		return member.toString();
	}

	public boolean canReach(final Node node) {

		// the node can reach itself
		if (equals(node)) {
			return true;
		}
		// make sure there are no cycles
		else {
			calcDistances(null);
		}

		// check if it can reach any parent
		for (final Node parent : parents) {
			if (parent.canReach(node)) {
				return true;
			}
		}

		return false;
	}
}
