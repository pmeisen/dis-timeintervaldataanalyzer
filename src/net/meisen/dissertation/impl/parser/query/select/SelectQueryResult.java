package net.meisen.dissertation.impl.parser.query.select;

import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.parser.query.IQueryResult;

public class SelectQueryResult implements IQueryResult {

	private Bitmap filterResult;

	public Bitmap getFilterResult() {
		return filterResult;
	}

	public void setFilterResult(final Bitmap filterResult) {
		this.filterResult = filterResult;
	}
}
