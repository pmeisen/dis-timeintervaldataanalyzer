package net.meisen.dissertation.impl.parser.query.select.evaluator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import net.meisen.dissertation.exceptions.GroupEvaluatorException;
import net.meisen.dissertation.impl.parser.query.select.SelectGroup;
import net.meisen.dissertation.impl.parser.query.select.logical.GroupExpression;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

public class GroupEvaluator {
	private final MetaDataModel metaDataModel;

	protected class GroupValues implements Iterable<Descriptor<?, ?, ?>> {
		private final String modelId;
		private final List<Descriptor<?, ?, ?>> values = new ArrayList<Descriptor<?, ?, ?>>();

		public GroupValues(final String modelId) {
			this.modelId = modelId;
		}

		public void add(final Descriptor<?, ?, ?> value) {
			values.add(value);
		}

		public int size() {
			return values.size();
		}

		public String getDescriptorModelId() {
			return modelId;
		}

		@Override
		public Iterator<Descriptor<?, ?, ?>> iterator() {
			return values.iterator();
		}
	}

	public GroupEvaluator(final TidaModel model) {

		// get the metaDataModels
		metaDataModel = model.getMetaDataModel();
	}

	public Bitmap[] evaluateGroupExpression(final GroupExpression group) {
		generateGroups(group);

		return null;
	}

	protected List<SelectGroup> generateGroups(final GroupExpression group) {
		final List<GroupValues> descriptorValues = new ArrayList<GroupValues>();

		// get all the group values
		for (final String descId : group.getDescriptors()) {
			descriptorValues.add(getDescriptorValues(descId, group));
		}

		// combine the groups
		List<SelectGroup> result = null;
		for (final GroupValues groupValue : descriptorValues) {
			final List<SelectGroup> iterationResult = new ArrayList<SelectGroup>();

			if (result == null) {
				for (final Descriptor<?, ?, ?> desc : groupValue) {

					// create the group and add it if not excluded already
					final SelectGroup selectGroup = new SelectGroup(desc);
					if (!group.excludes(selectGroup)) {
						iterationResult.add(new SelectGroup(desc));
					}
				}
			} else {
				for (final SelectGroup selectGroup : result) {
					for (final Descriptor<?, ?, ?> desc : groupValue) {
						
						// create a new SelectGroup with the appended value
						final SelectGroup copy = new SelectGroup(selectGroup,
								desc);

						/*
						 * append the new descriptor and check only the new
						 * value for exclusion
						 */
						if (!group.excludes(copy)) {
							iterationResult.add(copy);
						}
					}
				}
			}

			// set the result for the next iteration
			result = iterationResult;

			// check if everything is filtered, nothing can be added
			if (result.size() == 0) {
				break;
			}
		}

		// null will be returned if nothing is defined for the group
		return result;
	}

	protected GroupValues getDescriptorValues(final String descId,
			final GroupExpression group) {
		final DescriptorModel<?> descModel = metaDataModel
				.getDescriptorModel(descId);
		if (descModel == null) {
			throw new ForwardedRuntimeException(GroupEvaluatorException.class,
					1000, descId);
		}

		final int position = group.getPosition(descId);
		if (position == -1) {
			throw new ForwardedRuntimeException(GroupEvaluatorException.class,
					1001, group, descId);
		}

		// check the different values
		final GroupValues values = new GroupValues(descId);
		for (final Descriptor<?, ?, ?> desc : descModel.getAllDescriptors()) {
			values.add(desc);
		}

		return values;
	}
}
