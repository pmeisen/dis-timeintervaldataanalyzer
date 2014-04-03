package net.meisen.dissertation.impl.parser.query.select.logical;

import java.util.ArrayList;
import java.util.List;

import net.meisen.dissertation.impl.parser.query.select.DescriptorComperator;
import net.meisen.general.genmisc.types.Strings;

public class LogicalOperatorNode implements ITreeElement {

	private final List<ITreeElement> children;
	private final LogicalOperator operator;

	private LogicalOperatorNode parent;

	protected LogicalOperatorNode() {
		this.children = new ArrayList<ITreeElement>();
		this.parent = null;
		this.operator = null;
	}

	public LogicalOperatorNode(final LogicalOperatorNode parent) {
		this(parent, null);
	}

	public LogicalOperatorNode(final LogicalOperatorNode parent,
			final LogicalOperator operator) {
		if (parent == null) {
			throw new NullPointerException("The parent cannot be null.");
		}

		this.operator = operator;
		this.children = new ArrayList<ITreeElement>();
		this.parent = parent;
	}

	public LogicalOperator get() {
		return this.operator;
	}

	@Override
	public LogicalOperatorNode getParent() {
		return this.parent;
	}

	public ITreeElement getChild(final int i) {
		return children.get(i);
	}

	public List<ITreeElement> getChildren() {
		return new ArrayList<ITreeElement>(children);
	}

	public void attachChild(final DescriptorComperator dc) {
		attachChild(new DescriptorLeaf(this, dc));
	}

	public void attachChild(final ITreeElement treeElement) {
		this.children.add(treeElement);
	}

	public void attachChildFirst(final ITreeElement treeElement) {
		this.children.add(0, treeElement);
	}

	public void removeChild(final LogicalOperatorNode current) {
		this.children.remove(current);
	}

	@Override
	public String toString() {
		return operator + "("
				+ Strings.smartTrimSequence(this.children.toString(), "[")
				+ ")";
	}

	@Override
	public void setParent(final LogicalOperatorNode parent) {
		if (this.parent == null) {
			throw new IllegalStateException(
					"The parent of the root cannot be changed.");
		}
		this.parent = parent;
	}
}
