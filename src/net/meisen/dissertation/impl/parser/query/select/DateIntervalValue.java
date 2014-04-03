package net.meisen.dissertation.impl.parser.query.select;

import java.util.Date;

import net.meisen.general.genmisc.types.Dates;

import org.antlr.v4.runtime.tree.TerminalNode;

public class DateIntervalValue extends IntervalValue<Date> {

	public DateIntervalValue(final TerminalNode node) {
		this(Dates.isDate(node.getText(), Dates.GENERAL_TIMEZONE));
	}

	public DateIntervalValue(final Date value) {
		super(value);
	}

	@Override
	public Class<Date> getType() {
		return Date.class;
	}
}
