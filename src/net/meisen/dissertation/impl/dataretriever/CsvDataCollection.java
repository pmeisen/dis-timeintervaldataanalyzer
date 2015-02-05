package net.meisen.dissertation.impl.dataretriever;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.regex.Pattern;

import net.meisen.dissertation.model.dataretriever.DataCollection;
import net.meisen.dissertation.model.dataretriever.DataIterator;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Streams;

/**
 * A collection used to iterate over the data of a csv-file.
 * 
 * @author pmeisen
 * 
 */
public class CsvDataCollection extends DataCollection<String> {
	private final BufferedReader reader;
	private final String separator;

	private int valuePosition;

	/**
	 * Constructor used to create a collection for the specified {@code Reader}
	 * and the specified {@code separator}.
	 * 
	 * @param reader
	 *            the reader to read from
	 * @param separator
	 *            the separator
	 */
	public CsvDataCollection(final BufferedReader reader, final String separator) {
		this(reader, separator, null);
	}

	/**
	 * Constructor used to create a collection for the specified {@code Reader}
	 * and the specified {@code separator}.
	 * 
	 * @param reader
	 *            the reader to read from
	 * @param separator
	 *            the separator
	 * @param selector
	 *            the configuration used for the collection
	 */
	public CsvDataCollection(final BufferedReader reader,
			final String separator, final CsvDataSelector selector) {
		this.valuePosition = -1;
		this.reader = reader;
		this.separator = separator;

		// read the header and correct the valuePosition if needed
		final String[] names = readLine();
		if (names == null) {
			throw new ForwardedRuntimeException(
					CsvDataRetrieverException.class, 1009);
		} else if (selector != null) {
			int position = -1;
			if (selector.getColumn() != null) {
				for (int i = 0; i < names.length; i++) {
					if (selector.getColumn().equals(names[i])) {
						position = i;
						break;
					}
				}
			} else {
				position = selector.getPosition() - 1;
			}

			if (position < 0 && names.length >= position) {
				throw new ForwardedRuntimeException(
						CsvDataRetrieverException.class, 1010, selector);
			} else if (selector.getPosition() < 1) {
				// found a valid position
			} else if (position != selector.getPosition() - 1) {
				throw new ForwardedRuntimeException(
						CsvDataRetrieverException.class, 1011, selector);
			}

			this.setNames(new String[] { names[position] });
			this.valuePosition = position;
		} else {
			this.setNames(names);
			this.valuePosition = -1;
		}
	}

	/**
	 * Reads a line of the csv-file and determines the different values.
	 * 
	 * @return the read values
	 */
	protected String[] readLine() {
		String line = null;
		try {
			do {
				line = reader.readLine();
			} while (line != null && line.trim().isEmpty());
		} catch (final IOException e) {
			throw new ForwardedRuntimeException(
					CsvDataRetrieverException.class, 1008, e);
		}

		if (line == null) {
			return null;
		} else {
			final String[] values = line.split(Pattern.quote(separator));
			if (this.valuePosition > -1) {
				return new String[] { values[this.valuePosition] };
			} else {
				return values;
			}
		}
	}

	@Override
	public DataIterator<String> iterator() {
		return new CsvDataIterator(this);
	}

	@Override
	public void release() {
		if (reader != null) {
			Streams.closeIO(reader);
		}
	}

}
