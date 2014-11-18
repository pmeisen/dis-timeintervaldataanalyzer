package net.meisen.dissertation.model.dimensions.graph;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import net.meisen.dissertation.exceptions.DescriptorDimensionException;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.dimensions.DescriptorDimension;
import net.meisen.dissertation.model.dimensions.DescriptorHierarchy;
import net.meisen.dissertation.model.dimensions.DescriptorLevel;
import net.meisen.dissertation.model.dimensions.DescriptorMember;
import net.meisen.dissertation.model.dimensions.IDimension;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * A graph representing and validating a defined {@code DescriptorDimension}.
 * 
 * @author pmeisen
 * 
 */
public class DescriptorDimensionGraph implements IDimensionGraph {

	private DescriptorDimension dimension = null;

	private final Map<String, Node> nodes;
	private List<Node> sourceNodes;
	private List<Node> sinkNode;

	private final Map<String, Level> levels;
	private List<Level> levelLeafs;
	private Level levelRoot;

	private boolean validated;

	/**
	 * The default constructor.
	 */
	public DescriptorDimensionGraph() {
		this.nodes = new HashMap<String, Node>();
		this.levels = new HashMap<String, Level>();

		this.sourceNodes = new ArrayList<Node>();
		this.sinkNode = null;

		this.levelLeafs = new ArrayList<Level>();
		this.levelRoot = null;

		this.validated = false;
	}

	/**
	 * Gets the node for the specified member. A member is uniquely defined by
	 * it's hierarchy and it's identifiers.
	 * 
	 * @param hierarchyId
	 *            the identifier of the {@code DescriptorHierarchy} the member
	 *            belongs to
	 * @param memberId
	 *            the identifier of the {@code DescriptorMember} to be retrieved
	 * 
	 * @return the {@code Node} of the member or {@code null} if no node for the
	 *         specified information exist
	 */
	public Node getNode(final String hierarchyId, final String memberId) {
		final String nodeId = createNodeId(hierarchyId, memberId);
		return nodes.get(nodeId);
	}

	/**
	 * Gets all the nodes of the {@code DescriptorDimensionGraph}.
	 * 
	 * @return all the nodes of the {@code DescriptorDimensionGraph}
	 */
	public Collection<Node> getNodes() {
		return Collections.unmodifiableCollection(nodes.values());
	}

	/**
	 * Gets all the levels of the {@code DescriptorDimensionGraph}.
	 * 
	 * @return all the levels of the {@code DescriptorDimensionGraph}
	 */
	public Collection<Level> getLevels() {
		return Collections.unmodifiableCollection(levels.values());
	}

	/**
	 * Gets a specific {@code Level} of the graph. The level is uniquely defined
	 * by the hierarchy it belongs to (which can be {@code null} if it's a
	 * shared level) and it's identifier used within the {@code DescriptorLevel}
	 * .
	 * 
	 * @param hierarchyId
	 *            the identifier of the hierarchy
	 * @param descLevelId
	 *            the identifier of the {@code DescriptorLevel}
	 * 
	 * @return the {@code Level} found or {@code null} if no {@code Level} for
	 *         the specified values exists
	 */
	public Level getLevel(final String hierarchyId, final String descLevelId) {
		final String levelId = createLevelId(hierarchyId, descLevelId);
		return levels.get(levelId);
	}

	/**
	 * Gets all the sources, a.k.a. leafs, of the graph.
	 * 
	 * @return the sources, a.k.a. leafs, of the graph
	 */
	public List<Node> getSources() {
		if (this.sourceNodes == null) {
			validate();
		}

		return this.sourceNodes;
	}

	/**
	 * Gets the sinks, a.k.a. roots, of the graph.
	 * 
	 * @return the sinks, a.k.a. roots, of the graph
	 */
	public List<Node> getSinks() {
		if (this.sinkNode == null) {
			validate();
		}

		return this.sinkNode;
	}

	@Override
	public void create(final IDimension dimension)
			throws ForwardedRuntimeException {

		// make sure the dimension is correct
		if (dimension instanceof DescriptorDimension == false) {
			throw new ForwardedRuntimeException(
					DescriptorDimensionException.class, 1016,
					dimension == null ? null : dimension.getClass()
							.getSimpleName());
		}

		this.validated = false;

		this.nodes.clear();
		this.levels.clear();

		this.sourceNodes.clear();
		this.sinkNode = null;

		this.levelLeafs.clear();
		this.levelRoot = null;

		this.dimension = (DescriptorDimension) dimension;

		final Collection<DescriptorHierarchy> hierarchies = this.dimension
				.getHierarchies();
		for (final DescriptorHierarchy hierarchy : hierarchies) {
			final String hierarchyId = hierarchy.getId();
			final Collection<DescriptorMember> members = hierarchy.getMembers();

			// check the different members
			for (final DescriptorMember member : members) {

				// create a node
				Node node = getOrCreateNode(hierarchyId, member);

				// get the level
				final Level level = getOrCreateLevel(member.getLevel());

				// add the node to the level
				level.add(node);

				// add the parents and the children
				for (final DescriptorMember parent : member.getRollUpTo()) {

					/*
					 * Do the nodes.
					 */
					final Node parentNode = getOrCreateNode(hierarchyId, parent);
					node.addParent(parentNode);

					// register this as child
					parentNode.addChild(node);

					/*
					 * Do the levels.
					 */
					final Level parentLevel = getOrCreateLevel(parentNode
							.getLevel());
					level.addParent(parentLevel);

					// register this as child
					parentLevel.addChild(level);
				}
			}
		}

		validate();
	}

	@Override
	public Bitmap createFilter(final String filterExpression,
			final TidaModel model) {
		return null;
	}

	@Override
	public Map<String, Bitmap> createGroup(final String groupExpression,
			final TidaModel model) {
		return null;
	}

	/**
	 * Gets or creates the level for the specified {@code descLevel}.
	 * 
	 * @param descLevel
	 *            the {@code DescriptorLevel} to create or get the {@code Level}
	 *            for
	 * 
	 * @return the {@code DescriptorLevel} created or retrieved
	 */
	protected Level getOrCreateLevel(final DescriptorLevel descLevel) {
		final String descLevelId = descLevel.getId();
		final String levelId = createLevelId(descLevel.getHierachy().getId(),
				descLevelId);

		Level level = levels.get(levelId);
		if (level == null) {
			level = new Level(dimension.isSharedLevel(descLevelId));
			levels.put(levelId, level);
		}

		// bind the DescriptorLevel to it
		level.bind(descLevel);

		return level;
	}

	/**
	 * Gets or creates the node for the specified {@code hierarchyId} and
	 * {@code DescriptorMember}.
	 * 
	 * @param hierarchyId
	 *            the identifier of the hierarchy the {@code Node} belongs to
	 * @param member
	 *            the {@code DescriptorMember} to get or create the {@code Node}
	 *            for
	 * 
	 * 
	 * @return the {@code DescriptorLevel} created or retrieved
	 */
	protected Node getOrCreateNode(final String hierarchyId,
			final DescriptorMember member) {

		final String nodeId = createNodeId(hierarchyId, member.getId());
		Node node = nodes.get(nodeId);

		if (node == null) {

			// create the node
			node = new Node(member);
			nodes.put(nodeId, node);
		}

		return node;
	}

	/**
	 * Creates the identifier used internally to represent the node for the
	 * specified {@code hierarchyId} and {@code memberId}.
	 * 
	 * @param hierarchyId
	 *            the identifier of the hierarchy
	 * @param memberId
	 *            the identifier of the member
	 * 
	 * @return the created internally used identifier
	 */
	protected String createNodeId(final String hierarchyId,
			final String memberId) {
		return hierarchyId + "." + memberId;
	}

	/**
	 * Creates the identifier used internally to represent the level for the
	 * specified {@code hierarchyId} and {@code levelId}.
	 * 
	 * @param hierarchyId
	 *            the identifier of the hierarchy
	 * @param levelId
	 *            the identifier of the level
	 * 
	 * @return the created internally used identifier
	 */
	protected String createLevelId(final String hierarchyId,
			final String levelId) {

		// check if we have a shared dimension
		final boolean shared = dimension.isSharedLevel(levelId);

		if (shared) {
			return levelId;
		} else {
			return hierarchyId + "." + levelId;
		}
	}

	/**
	 * Validates the graph. This method is internally called after creation.
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the validation fails
	 */
	protected void validate() throws ForwardedRuntimeException {

		if (validated) {
			return;
		} else {
			validateNodes();
			validateLevels();

			validated = true;
		}
	}

	/**
	 * Validates the nodes: There is only one sink (a.k.a. root), the sink is
	 * reachable from every node, and every source is a matcher (descriptor's
	 * value).
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the validation fails
	 */
	protected void validateNodes() throws ForwardedRuntimeException {

		// find the sink and the sources
		for (final Node node : nodes.values()) {
			final DescriptorMember member = node.getMember();

			// the member cannot be null
			if (member == null) {
				throw new ForwardedRuntimeException(
						DescriptorDimensionException.class, 1002);
			}

			// check if we have a sink
			if (node.isSink()) {

				if (node.getMinDistance() != 0) {
					throw new ForwardedRuntimeException(
							DescriptorDimensionException.class, 1016,
							dimension.getId(), node.getMinDistance());
				} else if (this.sinkNode == null) {
					this.sinkNode = new ArrayList<Node>();
				}
				this.sinkNode.add(node);
			} else if (node.getMinDistance() < 0) {
				throw new ForwardedRuntimeException(
						DescriptorDimensionException.class, 1017, node
								.getMember().getId(), dimension.getId(),
						node.getMinDistance());
			}

			// check if we have a pattern based, none source
			if (node.isSource()) {
				this.sourceNodes.add(node);
			} else if (member.isPatternBased()) {
				throw new ForwardedRuntimeException(
						DescriptorDimensionException.class, 1005,
						member.getId());
			}
		}

		// make sure we found a sink
		if (this.sinkNode == null) {
			throw new ForwardedRuntimeException(
					DescriptorDimensionException.class, 1009, dimension.getId());
		} else if (this.sourceNodes.isEmpty()) {
			throw new ForwardedRuntimeException(
					DescriptorDimensionException.class, 1013, dimension.getId());
		}
	}

	/**
	 * Validates the levels: The levels must provide a partial order over a
	 * partition of all the nodes.
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the validation fails
	 */
	protected void validateLevels() throws ForwardedRuntimeException {

		// find the root and the leafs
		final Set<String> emptyLevels = new HashSet<String>();
		for (final Entry<String, Level> entry : this.levels.entrySet()) {

			// get the level
			final Level level = entry.getValue();

			// check if the level is empty, it will be removed afterwards
			if (level.isEmpty()) {
				emptyLevels.add(entry.getKey());
			} else {

				// check if we have a root level
				if (level.isRoot()) {
					if (this.levelRoot != null) {
						throw new ForwardedRuntimeException(
								DescriptorDimensionException.class, 1011,
								dimension.getId());
					}

					this.levelRoot = level;
				}

				// check leafs
				if (level.isLeaf()) {
					this.levelLeafs.add(level);
				}

				/*
				 * Check the parents, all parents are by definition in relation
				 * to this: this < allParents
				 */
				final int levelMaxDistance = level.getMaxDistance();
				for (final Level parent : level.getAllParents()) {

					/*
					 * Make sure we get closer, anyways only the max has to be
					 * reduced compared to the min.
					 */
					if (levelMaxDistance <= parent.getMinDistance()) {
						throw new ForwardedRuntimeException(
								DescriptorDimensionException.class, 1019,
								level.getDescriptorLevelId(), levelMaxDistance,
								parent.getDescriptorLevelId(),
								parent.getMinDistance());
					}

					boolean reachable = false;
					for (final Node node : level.getNodes()) {
						for (final Node parentNode : parent.getNodes()) {
							if (node.canReach(parentNode)) {
								reachable = true;
								break;
							}

							/*
							 * The condition
							 * "For all parentNodes there exists no node in this level which can be reached"
							 * does not have to be checked, because we received
							 * all parents already. If all parents can be
							 * received, there is no cycle defined.
							 */
							// if (parentNode.canReach(node)) {
							// throw exception
							// }
						}
					}

					// throw the exception if no path was found
					if (!reachable) {
						throw new ForwardedRuntimeException(
								DescriptorDimensionException.class, 1020,
								level.getDescriptorLevelId(),
								parent.getDescriptorLevelId());
					}
				}
			}
		}

		// remove the empty levels
		for (final String key : emptyLevels) {
			this.levels.remove(key);
		}

		// make sure we found a root
		if (this.levelRoot == null) {
			throw new ForwardedRuntimeException(
					DescriptorDimensionException.class, 1011, dimension.getId());
		} else if (this.levelLeafs.isEmpty()) {
			throw new ForwardedRuntimeException(
					DescriptorDimensionException.class, 1012, dimension.getId());
		}
	}

	@Override
	public DescriptorDimension getDimension() {
		return dimension;
	}
}
