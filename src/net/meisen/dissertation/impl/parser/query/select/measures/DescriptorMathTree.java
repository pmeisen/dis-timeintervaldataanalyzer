package net.meisen.dissertation.impl.parser.query.select.measures;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.UUID;

import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.measures.IAggregationFunction;

/**
 * Logic representing a filter definition, based on values of {@code Descriptor}
 * instances.
 * 
 * @author pmeisen
 * 
 * @see Descriptor
 * 
 */
public class DescriptorMathTree {
	private final String id;
	private final RootNode root;

	private MathOperatorNode currentNode;

	/**
	 * Constructor to create an empty math-tree.
	 */
	public DescriptorMathTree() {
		this(null);
	}

	/**
	 * Constructor to create an empty math-tree with the specified {@code id}.
	 * 
	 * @param id
	 *            the identifier used for the math-tree, can be {@code null} (if
	 *            so a random identifier will be created)
	 */
	public DescriptorMathTree(final String id) {
		this.root = new RootNode();
		this.currentNode = root;
		this.id = id == null ? UUID.randomUUID().toString() : id;
	}

	/**
	 * Gets the root of the tree.
	 * 
	 * @return the root of the tree
	 */
	public MathOperatorNode getRoot() {
		return root;
	}

	/**
	 * Attaches the specified {@code MathOperator} to the tree. The
	 * {@code MathOperator} is attached as {@code MathOperatorNode} to the
	 * current node. The <b>current node is changed</b> to be the attached node.
	 * 
	 * @param operator
	 *            the {@code MathOperator} to be attached
	 */
	public void attach(final MathOperator operator) {

		// add a new logicNode
		final MathOperatorNode mn = new MathOperatorNode(this.currentNode,
				operator);
		this.currentNode.attachChild(mn);
		this.currentNode = mn;
	}

	/**
	 * Attaches the specified {@code ArithmeticOperator} to the tree. The
	 * {@code ArithmeticOperator} is attached as {@code MathOperatorNode} to the
	 * current node. The <b>current node is changed</b> to be the attached node.
	 * 
	 * @param operator
	 *            the {@code ArithmeticOperator} to be attached
	 */
	public void attach(final ArithmeticOperator operator) {
		attach(new MathOperator(operator));
	}

	/**
	 * Attaches the specified {@code AggregationFunction} to the tree. The
	 * {@code AggregationFunction} is attached as {@code MathOperatorNode} to
	 * the current node. The <b>current node is changed</b> to be the attached
	 * node.
	 * 
	 * @param function
	 *            the {@code AggregationFunction} to be attached
	 * 
	 * @see IAggregationFunction
	 */
	public void attach(final IAggregationFunction function) {
		attach(new MathOperator(function));
	}

	/**
	 * Attaches the specified {@code Descriptor} to the tree. The
	 * {@code Descriptor's} identifier is attached as is to the current node.
	 * The <b>current node is not changed</b> to be the attached node.
	 * 
	 * @param descriptorId
	 *            the identifier of the {@code Descriptor} to be attached
	 */
	public void attach(final String descriptorId) {
		this.currentNode.attachChild(descriptorId);
	}

	/**
	 * Moves the current node up to the parent of the current node. If the
	 * current node is the root node, an exception is thrown.
	 * 
	 * @throws IllegalStateException
	 *             if the method is called and the current node is the root node
	 */
	public void moveUp() throws IllegalStateException {
		final MathOperatorNode parent = this.currentNode.getParent();
		if (parent == null) {
			throw new IllegalStateException(
					"Cannot move up anymore, the root is reached");
		} else {
			this.currentNode = parent;
		}
	}

	/**
	 * Creates a list of elements of the tree ordered by evaluation order of the
	 * different nodes of the tree.
	 * 
	 * @return a list of elements of the tree ordered by evaluation order
	 */
	public List<IMathTreeElement> getEvaluationOrder() {
		final List<IMathTreeElement> order = new ArrayList<IMathTreeElement>();

		// check if the root has children, otherwise there is nothing to do
		if (root.getChildren().size() != 0) {

			final Deque<IMathTreeElement> stack = new ArrayDeque<IMathTreeElement>();
			fillStack(root, stack);

			while (!stack.isEmpty()) {
				order.add(stack.pop());
			}
		}

		return order;
	}

	/**
	 * Helper method to create the evaluation ordered list.
	 * 
	 * @param node
	 *            the current node used to fill the stack
	 * @param stack
	 *            the stack to be filled
	 */
	protected void fillStack(final MathOperatorNode node,
			final Deque<IMathTreeElement> stack) {

		final List<IMathTreeElement> children = node.getChildren();
		for (int i = children.size(); i > 0; i--) {
			final IMathTreeElement child = children.get(i - 1);

			stack.push(child);
			if (child instanceof MathOperatorNode) {
				fillStack((MathOperatorNode) child, stack);
			}
		}
	}

	@Override
	public String toString() {
		return root.toString();
	}

	/**
	 * Checks if the tree is simple, i.e. just a aggregation function for a
	 * descriptor (e.g. {@code SUM(SMILES)}).
	 * 
	 * @return {@code true} if the tree represents a simple term, otherwise
	 *         {@code false}
	 */
	public boolean isSimple() {
		final IMathTreeElement c = getFirst();

		if (c instanceof MathOperatorNode) {
			final MathOperatorNode child = (MathOperatorNode) c;
			final MathOperator op = child.get();
			return (op.isFunction() && child.amountOfChildren() == 1 && child
					.getChild(0) instanceof DescriptorLeaf);
		} else {
			return false;
		}
	}

	/**
	 * Gets the identifier of the math-tree.
	 * 
	 * @return the identifier of the math-tree
	 */
	public String getId() {
		return id;
	}

	/**
	 * Gets the first none root node of the tree.
	 * 
	 * @return the first none root node of the tree
	 */
	public IMathTreeElement getFirst() {
		if (root.amountOfChildren() == 0) {
			return null;
		} else {
			return root.getChild(0);
		}
	}

	/**
	 * Checks if a function of the tree uses the specified usage-type of
	 * function.
	 * 
	 * @param type
	 *            the type to be checked
	 * 
	 * @return {@code true} if a function uses the specified function (i.e.
	 *         {@link IAggregationFunction#getDefinedType()} is equal to the
	 *         specified {@code type}), otherwise {@code false}
	 */
	public boolean usesFunction(final Class<? extends IAggregationFunction> type) {
		return usesFunction(type, root);
	}

	/**
	 * Helper method to recursively check the tree (i.e. specified sub-tree) for
	 * a function of the specified {@code type}.
	 * 
	 * @param type
	 *            the type to be checked
	 * @param node
	 *            the node to start at
	 * 
	 * @return {@code true} if a function uses the specified function (i.e.
	 *         {@link IAggregationFunction#getDefinedType()} is equal to the
	 *         specified {@code type}), otherwise {@code false}
	 */
	protected boolean usesFunction(
			final Class<? extends IAggregationFunction> type,
			final MathOperatorNode node) {
		if (type == null) {
			return false;
		}

		final List<IMathTreeElement> children = node.getChildren();
		for (final IMathTreeElement child : children) {

			if (child instanceof MathOperatorNode) {
				final MathOperatorNode mon = (MathOperatorNode) child;
				final MathOperator mo = mon.get();
				final IAggregationFunction func = mo.getFunction();

				if (func != null && type.equals(func.getDefinedType())) {
					return true;
				} else if (usesFunction(type, mon)) {
					return true;
				}
			}
		}

		return false;
	}
}
