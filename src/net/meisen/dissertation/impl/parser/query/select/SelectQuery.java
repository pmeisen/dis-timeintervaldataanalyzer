package net.meisen.dissertation.impl.parser.query.select;

import java.util.List;

import net.meisen.dissertation.impl.parser.query.select.logical.DescriptorLeaf;
import net.meisen.dissertation.impl.parser.query.select.logical.DescriptorLogicTree;
import net.meisen.dissertation.impl.parser.query.select.logical.ITreeElement;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.parser.query.IQuery;

public class SelectQuery implements IQuery {

	private final DescriptorLogicTree filter;

	private String modelId;
	private ResultType type;
	private Interval<?> interval;

	public SelectQuery() {
		filter = new DescriptorLogicTree();
	}

	public void setResultType(final ResultType type) {
		this.type = type;
	}

	public ResultType getResultType() {
		return type;
	}

	@Override
	public String toString() {
		return "select " + type + " in " + interval + " filter " + filter;
	}

	public Interval<?> getInterval() {
		return interval;
	}

	public void setInterval(final Interval<?> interval) {
		this.interval = interval;
	}

	public DescriptorLogicTree getFilter() {
		return filter;
	}

	public void optimize() {
		filter.optimize();
	}

	@Override
	public void execute(final TidaModel model) {

		// get the filters
		final List<ITreeElement> filters = filter.getEvaluationOrder();

		// get the metaDataModels
		final MetaDataModel metaDataModel = model.getMetaDataModel();

		// now look for the models and the values
		for (final ITreeElement te : filters) {
			if (te instanceof DescriptorLeaf) {
				final DescriptorLeaf leaf = (DescriptorLeaf) te;
				final DescriptorComperator cmp = leaf.get();

				final DescriptorModel<?> descModel = metaDataModel
						.getDescriptorModel(cmp.getId());
				if (descModel == null) {
					// TODO throw exception
					throw new IllegalArgumentException(
							"A metaDataModel with id '" + cmp.getId()
									+ "' doesn't exist.");
				}

				final Descriptor<?, ?, ?> desc = descModel
						.getDescriptorByValue(cmp.getValue());
				if (desc == null) {
					// TODO throw exception
					throw new IllegalArgumentException(
							"A descriptor with the value '" + cmp.getValue()
									+ "' could not be found.");
				}
			}
		}

		final TidaIndex index = model.getIndex();

	}

	@Override
	public String getModelId() {
		return modelId;
	}

	public void setModelId(final String modelId) {
		this.modelId = modelId;
	}
}
