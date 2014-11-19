package net.meisen.dissertation.impl.parser.query.delete;

import gnu.trove.list.array.TIntArrayList;

import java.util.Collection;

import net.meisen.dissertation.jdbc.protocol.QueryType;
import net.meisen.dissertation.model.auth.IAuthManager;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.dissertation.model.auth.permissions.Permission;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IQueryResult;
import net.meisen.dissertation.model.parser.query.IResourceResolver;
import net.meisen.dissertation.server.CancellationException;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * A query used to delete specified records.
 * 
 * @author pmeisen
 * 
 */
public class DeleteQuery implements IQuery {
	private String modelId;
	private boolean enableIdCollection;

	private TIntArrayList idList = new TIntArrayList();

	/**
	 * Constructor used to delete specific queries from the database.
	 */
	public DeleteQuery() {
		this.enableIdCollection(false);
	}

	@Override
	public boolean expectsModel() {
		return true;
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
	public IQueryResult evaluate(final IAuthManager authManager,
			final TidaModelHandler handler, final TidaModel model,
			final IResourceResolver resolver) throws ForwardedRuntimeException,
			CancellationException {

		final int[] ids = model.invalidateRecords(idList.toArray());
		if (isEnableIdCollection()) {
			return new DeleteResult(ids);
		} else {
			return new DeleteResult(ids.length);
		}
	}

	/**
	 * Sets the list of integers to be deleted
	 * 
	 * @param idList
	 *            the integers to be deleted
	 */
	public void setIdList(final Collection<Integer> idList) {
		this.idList.addAll(idList);
	}

	/**
	 * Sets the list of identifiers to be deleted.
	 * 
	 * @param ids
	 *            the list of identifiers to be deleted
	 */
	public void setIdList(final int[] ids) {
		this.idList.clear();
		this.idList.addAll(ids);
	}

	/**
	 * Gets the list of identifiers to be deleted.
	 * 
	 * @return the list of identifiers to be deleted
	 */
	public int[] getIdList() {
		return this.idList.toArray();
	}

	@Override
	public String toString() {
		return "DELETE " + this.idList.toString() + " FROM " + getModelId();
	}

	@Override
	public QueryType getQueryType() {
		return QueryType.MANIPULATION;
	}

	@Override
	public void enableIdCollection(final boolean enableIdCollection) {
		this.enableIdCollection = enableIdCollection;
	}

	/**
	 * Checks if the collection of identifiers is enabled.
	 * 
	 * @return {@code true} if the collection is enabled, otherwise
	 *         {@code false}
	 */
	public boolean isEnableIdCollection() {
		return enableIdCollection;
	}

	@Override
	public DefinedPermission[][] getNeededPermissions() {
		return new DefinedPermission[][] {
				new DefinedPermission[] { Permission.modify.create(modelId) },
				new DefinedPermission[] { Permission.modifyAll.create() } };
	}

}
