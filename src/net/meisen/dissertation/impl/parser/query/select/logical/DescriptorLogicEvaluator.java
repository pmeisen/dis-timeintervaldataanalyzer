package net.meisen.dissertation.impl.parser.query.select.logical;

import java.util.ArrayList;
import java.util.List;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.impl.parser.query.select.DescriptorComperator;
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

public class DescriptorLogicEvaluator {
	private final static Logger LOG = LoggerFactory
			.getLogger(DescriptorLogicEvaluator.class);

	private final BaseIndexFactory indexFactory;
	private final TidaIndex index;
	private final MetaDataModel metaDataModel;

	public DescriptorLogicEvaluator(final TidaModel model) {

		// get the metaDataModels
		metaDataModel = model.getMetaDataModel();

		// get the index
		index = model.getIndex();

		// get the indexFactory
		indexFactory = model.getIndexFactory();
	}

	public Bitmap evaluateTree(final DescriptorLogicTree tree) {
		final LogicalOperatorNode root = tree.getRoot();

		if (LOG.isDebugEnabled()) {
			LOG.debug("Evaluating logic-tree '" + tree + "'.");
		}

		// check if the root has children, otherwise there is nothing to do
		if (tree.getRoot().getChildren().size() != 0) {
			return evaluateNode(root, this);
		} else {
			return null;
		}
	}

	protected Bitmap evaluateNode(final LogicalOperatorNode node,
			final DescriptorLogicEvaluator evaluator) {
		final List<Bitmap> members = new ArrayList<Bitmap>();

		final List<ITreeElement> children = node.getChildren();
		for (int i = children.size(); i > 0; i--) {
			final ITreeElement child = children.get(i - 1);

			if (child instanceof LogicalOperatorNode) {
				members.add(evaluateNode((LogicalOperatorNode) child, evaluator));
			} else if (child instanceof DescriptorLeaf) {
				members.add(evaluator.evaluate((DescriptorLeaf) child));
			} else {
				throw new ForwardedRuntimeException(
						QueryEvaluationException.class, 1006, child);
			}
		}

		final Bitmap evalResult = evaluator.evaluateOperator(node.get(), members);

		if (LOG.isDebugEnabled()) {
			LOG.debug("Evaluated logic '" + node.get() + "' for members '"
					+ members + "' with result '" + evalResult + "'.");
		}

		return evalResult;
	}

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

	protected Bitmap evaluate(final DescriptorLeaf leaf) {

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
				return slice.getBitmap();
			}
		}
	}

}
