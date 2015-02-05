package net.meisen.dissertation.impl.dataretriever;

import net.meisen.dissertation.model.dataretriever.IQueryConfiguration;

/**
 * The configuration of a {@code CsvDataCollection}, i.e. a query instance for a
 * csv-files.
 * 
 * @author pmeisen
 * 
 */
public class CsvDataSelector implements IQueryConfiguration {
	private String column;
	private int position;

	/**
	 * Default constructor.
	 */
	public CsvDataSelector() {
		this.column = null;
		this.position = -1;
	}

	/**
	 * Gets the column defined to retrieve the values from. Generally, it is
	 * enough to define just a column or a position.
	 * 
	 * @return the column defined to retrieve the values from
	 * 
	 * @see #setPosition(int)
	 */
	public String getColumn() {
		return column;
	}

	/**
	 * Sets the column to retrieve the values from.
	 * 
	 * @param column
	 *            the column to retrieve the values from
	 */
	public void setColumn(String column) {
		this.column = column;
	}

	/**
	 * Gets the position to get the value from
	 * 
	 * @return the position to get the value from
	 */
	public int getPosition() {
		return position;
	}

	/**
	 * Sets the position to retrieve the values from. Generally, it is enough to
	 * define just a column or a position.
	 * 
	 * @param position
	 *            the position to retrieve the values from
	 * 
	 * @see #setColumn(String)
	 */
	public void setPosition(int position) {
		this.position = position;
	}

	@Override
	public String toString() {
		if (column == null && position < 1) {
			return null;
		} else if (column != null && position > 0) {
			return column + " (" + position + ")";
		} else if (position > 0) {
			return "" + position;
		} else if (column != null) {
			return column;
		} else {
			return null;
		}
	}
}
