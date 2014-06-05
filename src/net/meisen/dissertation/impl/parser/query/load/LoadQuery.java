package net.meisen.dissertation.impl.parser.query.load;

import java.io.InputStream;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.exceptions.TidaModelHandlerException;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IQueryResult;
import net.meisen.dissertation.model.parser.query.IResourceResolver;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Streams;

/**
 * A query used to load a {@code TidaModel}.
 * 
 * @author pmeisen
 * 
 */
public class LoadQuery implements IQuery {

	private String modelId;
	private String path;

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
			final TidaModel model, final IResourceResolver resolver) {

		final String modelId;
		if (getPath() == null) {
			modelId = getModelId();

			// check if the model is already loaded
			if (handler.getTidaModel(modelId) != null) {
				throw new ForwardedRuntimeException(
						QueryEvaluationException.class, 1013, modelId);
			}
			// let's load the model otherwise
			else {
				try {
					handler.loadFromDefaultLocation(modelId);
				} catch (final TidaModelHandlerException e) {
					throw new ForwardedRuntimeException(
							QueryEvaluationException.class, 1012, e, modelId);
				}
			}
		} else if (resolver == null) {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1014, getPath());
		} else {
			final InputStream is = resolver.resolve(getPath());
			final TidaModel loadedModel = handler.loadViaXslt(is);
			Streams.closeIO(is);

			modelId = loadedModel.getId();
		}

		// create the result, the loading is finished
		final LoadResult result = new LoadResult();
		result.setModelId(modelId);

		return result;
	}

	/**
	 * Gets the specified path (of the client) to load the model from.
	 * 
	 * @return the path to load the model from (on client-side)
	 */
	public String getPath() {
		return path;
	}

	/**
	 * Sets the specified path (of the client) to load the model from.
	 * 
	 * @param path
	 *            the path to load the model from (on client-side)
	 */
	public void setPath(final String path) {
		this.path = path;
	}

	@Override
	public boolean expectsModel() {
		return false;
	}

	@Override
	public String toString() {
		return "load " + getModelId()
				+ (getPath() == null ? "" : " from " + getPath());
	}
}
