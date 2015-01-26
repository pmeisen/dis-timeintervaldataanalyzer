package net.meisen.dissertation.performance.paper.in2014.iceis;

import java.util.List;
import java.util.Map;

public interface IRecordsFilter {

	public List<Map<String, Object>> apply(final long start, final long end);

	public List<Map<String, Object>> apply(final long start, final long end,
			final List<Map<String, Object>> records);

	public boolean incSupport();
}
