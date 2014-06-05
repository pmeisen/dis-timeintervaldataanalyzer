package net.meisen.dissertation.impl.parser.query.load;

import net.meisen.dissertation.model.parser.query.IQueryResult;

/**
 * The result provided by a {@code LoadQuery}.
 * 
 * @author pmeisen
 * 
 * @see LoadQuery
 * 
 */
public class LoadResult implements IQueryResult {
	private String modelId;

	/**
	 * Gets the identifier of the loaded {@code TidaModel}.
	 * 
	 * @return the identifier of the loaded {@code TidaModel}
	 */
	public String getModelId() {
		return modelId;
	}

	/**
	 * Sets the identifier of the loaded {@code TidaModel}.
	 * 
	 * @param modelId
	 *            the identifier of the loaded {@code TidaModel}
	 */
	public void setModelId(final String modelId) {
		this.modelId = modelId;
	}

	@Override
	public String toString() {
		return modelId;
	}
}
