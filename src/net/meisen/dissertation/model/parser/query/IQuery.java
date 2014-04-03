package net.meisen.dissertation.model.parser.query;

import net.meisen.dissertation.model.data.TidaModel;

/**
 * A query instance used to retrieve data from the modeled {@code TidaSystem}.
 * 
 * @author pmeisen
 * 
 */
public interface IQuery {

	public String getModelId();
	
	public void execute(final TidaModel model);
}
