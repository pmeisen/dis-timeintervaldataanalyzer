package net.meisen.dissertation.parser.query.select.logical;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import net.meisen.dissertation.parser.query.select.DescriptorComperator;
import net.meisen.general.genmisc.types.Strings;

public class LogicalOperatorNode implements ITreeElement {

	private final LogicalOperatorNode parent;
	private final List<ITreeElement> children;
	private final LogicalOperator operator;

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
		return Collections.unmodifiableList(children);
	}

	public void attachChild(final DescriptorComperator dc) {
		this.children.add(new DescriptorLeaf(this, dc));
	}

	public void attachChild(final LogicalOperatorNode ln) {
		this.children.add(ln);
	}

	@Override
	public String toString() {
		return operator + "("
				+ Strings.smartTrimSequence(this.children.toString(), "[")
				+ ")";
	}
}
