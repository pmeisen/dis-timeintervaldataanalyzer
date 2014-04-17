package net.meisen.dissertation.impl.parser.query.select.logical;

import java.util.List;

import net.meisen.general.genmisc.types.Strings;

/**
 * The root of a {@code DescriptorLogicTree}.
 * 
 * @author pmeisen
 * 
 */
public class RootNode extends LogicalOperatorNode {

	@Override
	public String toString() {
		final List<ITreeElement> children = getChildren();

		if (children.size() == 0) {
			return "root";
		} else if (children.size() == 1) {
			return children.get(0).toString();
		} else {
			return "root" + "("
					+ Strings.smartTrimSequence(children.toString(), "[") + ")";
		}
	}
}
