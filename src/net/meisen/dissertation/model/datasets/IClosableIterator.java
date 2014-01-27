package net.meisen.dissertation.model.datasets;

public interface IClosableIterator {
	
	/**
	 * Closes the iterator, i.e. is not used anymore, so that resources used by
	 * the iterator can be freed.
	 */
	public void close();
}
