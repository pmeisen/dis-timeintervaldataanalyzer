package net.meisen.dissertation.impl.parser.query.unload;

import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IQueryResult;
import net.meisen.dissertation.model.parser.query.IResourceResolver;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * Query to unload a specific {@code TidaModel}.
 * 
 * @author pmeisen
 * 
 */
public class UnloadQuery implements IQuery {
	private String modelId;

	@Override
	public boolean expectsModel() {
		return false;
	}

	@Override
	public String getModelId() {
		return modelId;
	}

	@Override
	public void setModelId(final String modelId) {
		this.modelId = modelId;
	}

	@Override
	public IQueryResult evaluate(final TidaModelHandler handler,
			final TidaModel model, final IResourceResolver resolver)
			throws ForwardedRuntimeException {

		// unload the model
		handler.unload(modelId);

		return new UnloadResult();
	}
}
