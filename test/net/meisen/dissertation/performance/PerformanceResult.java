package net.meisen.dissertation.performance;

public class PerformanceResult {
	public int limit;
	public int selected;
	public int size;
	public String impl;
	public String query;
	public long avg;
	public long min;
	public long max;
	public int runs;

	public PerformanceResult() {
	}

	public PerformanceResult(int limit, int selected, int size, String impl,
			String query) {
		this.limit = limit;
		this.selected = selected;
		this.size = size;
		this.impl = impl;
		this.query = query;
	}

	public String csv() {
		return this.impl + "," + this.avg + "," + this.min + "," + this.max
				+ "," + this.limit + "," + this.selected + "," + this.size
				+ ",\"" + this.query + "\"";
	}
}
