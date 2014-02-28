package net.meisen.dissertation.parser.query.select;

import org.antlr.v4.runtime.tree.TerminalNode;

public class LongIntervalValue extends IntervalValue<Long> {

	public LongIntervalValue(final TerminalNode node) {
		this(Long.parseLong(node.getText()));
	}

	public LongIntervalValue(final Long value) {
		super(value);
	}

	@Override
	public Class<Long> getType() {
		return Long.class;
	}
}
