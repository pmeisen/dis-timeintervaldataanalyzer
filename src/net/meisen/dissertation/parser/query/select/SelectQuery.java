package net.meisen.dissertation.parser.query.select;

import net.meisen.dissertation.parser.query.IQuery;
import net.meisen.dissertation.parser.query.select.logical.DescriptorLogicTree;

public class SelectQuery implements IQuery {

	private ResultType type;
	private Interval<?> interval;
	private DescriptorLogicTree filter = new DescriptorLogicTree();

	public void setResultType(final ResultType type) {
		this.type = type;
	}

	public ResultType getResultType() {
		return type;
	}

	@Override
	public String toString() {
		final String newline = System.getProperty("line.separator");
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
		
	}
}
