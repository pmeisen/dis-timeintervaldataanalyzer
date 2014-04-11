package net.meisen.dissertation.impl.parser.query.select.evaluator;

import java.util.ArrayList;
import java.util.List;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.impl.parser.query.select.DescriptorComperator;
import net.meisen.dissertation.impl.parser.query.select.logical.DescriptorLeaf;
import net.meisen.dissertation.impl.parser.query.select.logical.DescriptorLogicTree;
import net.meisen.dissertation.impl.parser.query.select.logical.ITreeElement;
import net.meisen.dissertation.impl.parser.query.select.logical.LogicalOperator;
import net.meisen.dissertation.impl.parser.query.select.logical.LogicalOperatorNode;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.IndexDimensionSlice;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A evaluator to evaluate a {@code DescriptorLogicTree} based on a {@code TidaModel}.
 * 
 * @author pmeisen
 * 
 */
public class DescriptorLogicEvaluator {
	private final static Logger LOG = LoggerFactory
			.getLogger(DescriptorLogicEvaluator.class);

	private final BaseIndexFactory indexFactory;
	private final TidaIndex index;
	private final MetaDataModel metaDataModel;

	/**
	 * The evaluator
	 * 
	 * @param model
	 */
	public DescriptorLogicEvaluator(final TidaModel model) {

		// get the metaDataModels
		metaDataModel = model.getMetaDataModel();

		// get the index
		index = model.getIndex();

		// get the indexFactory
		indexFactory = model.getIndexFactory();
	}

	/**
	 * Calculates the bitmap representing the selected records of the specified
	 * filter, i.e. a single instance of the {@code Bitmap}. The created bitmap
	 * is an own instance, i.e. it is cloned or especially created using and/or
	 * operations.
	 * 
	 * @param tree
	 *            the filter to be applied
	 * 
	 * @return a bitmap representing the selected records
	 */
	public Bitmap evaluateTree(final DescriptorLogicTree tree) {
		final LogicalOperatorNode root = tree.getRoot();

		if (LOG.isDebugEnabled()) {
			LOG.debug("Evaluating logic-tree '" + tree + "'.");
		}

		// check if the root has children, otherwise there is nothing to do
		if (tree.getRoot().getChildren().size() != 0) {
			return evaluateNode(root);
		} else {
			return null;
		}
	}

	/**
	 * Evaluates a specific {@code node} of the tree.
	 * 
	 * @param node
	 *            the node to be evaluated
	 * 
	 * @return evaluates a specific {@code node} of the tree
	 */
	protected Bitmap evaluateNode(final LogicalOperatorNode node) {
		final List<Bitmap> members = new ArrayList<Bitmap>();

		final List<ITreeElement> children = node.getChildren();
		for (int i = children.size(); i > 0; i--) {
			final ITreeElement child = children.get(i - 1);

			if (child instanceof LogicalOperatorNode) {
				members.add(evaluateNode((LogicalOperatorNode) child));
			} else if (child instanceof DescriptorLeaf) {
				members.add(evaluateDescriptorLeaf((DescriptorLeaf) child));
			} else {
				throw new ForwardedRuntimeException(
						QueryEvaluationException.class, 1006, child);
			}
		}

		final Bitmap evalResult = evaluateOperator(node.get(), members);

		if (LOG.isDebugEnabled()) {
			LOG.debug("Evaluated logic '" + node.get() + "' for members '"
					+ members + "' with result '" + evalResult + "'.");
		}

		return evalResult;
	}

	/**
	 * Evaluates the result of a {@code LogicalOperator}.
	 * 
	 * @param lo
	 *            the {@code LogicalOperator} to be evaluated
	 * @param intermediateBitmaps
	 *            the current results
	 * 
	 * @return the result of the application of the {@code LogicalOperator}
	 */
	protected Bitmap evaluateOperator(final LogicalOperator lo,
			final List<Bitmap> intermediateBitmaps) {

		if (LogicalOperator.NOT.equals(lo)) {
			if (intermediateBitmaps.size() != 1) {
				throw new ForwardedRuntimeException(
						QueryEvaluationException.class, 1003,
						intermediateBitmaps);
			}

			// apply the not
			return intermediateBitmaps.get(0).invert(index.getNextDataId() - 1);
		} else {
			final int size = intermediateBitmaps.size();

			if (size < 1) {
				throw new ForwardedRuntimeException(
						QueryEvaluationException.class, 1004,
						intermediateBitmaps);
			} else if (size > 1) {
				if (LogicalOperator.AND.equals(lo)) {
					return Bitmap.and(indexFactory,
							intermediateBitmaps.toArray());
				} else if (LogicalOperator.OR.equals(lo)) {
					return Bitmap.or(indexFactory,
							intermediateBitmaps.toArray());
				} else {
					throw new ForwardedRuntimeException(
							QueryEvaluationException.class, 1005, lo);
				}
			} else {
				return intermediateBitmaps.get(0);
			}
		}
	}

	/**
	 * Evaluates the result of the selection of a {@code Descriptor}.
	 * 
	 * @param leaf
	 *            the {@code DescriptorLeaf} to be evaluated
	 * 
	 * @return the result of the selection of the {@code Descriptor}
	 */
	protected Bitmap evaluateDescriptorLeaf(final DescriptorLeaf leaf) {

		final DescriptorComperator cmp = leaf.get();

		// get the addressed model
		final DescriptorModel<?> descModel = metaDataModel
				.getDescriptorModel(cmp.getId());
		if (descModel == null) {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1002, cmp.getId());
		}

		// get the addressed descriptor
		final Descriptor<?, ?, ?> desc = descModel.getDescriptorByString(cmp
				.getValue());
		if (desc == null) {
			return indexFactory.createBitmap();
		} else {

			// use the index to retrieve the slice
			final IndexDimensionSlice<?> slice = index
					.getMetaIndexDimensionSlice(cmp.getId(), desc.getId());

			// check if we have a slice, otherwise we assume null
			if (slice == null) {
				return indexFactory.createBitmap();
			} else {
				return slice.getBitmap().copy();
			}
		}
	}

}