package net.meisen.dissertation.impl.dataretriever;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;

import net.meisen.dissertation.model.dataretriever.BaseDataRetriever;
import net.meisen.dissertation.model.dataretriever.DataCollection;
import net.meisen.dissertation.model.dataretriever.IDataRetrieverConfig;
import net.meisen.dissertation.model.dataretriever.IQueryConfiguration;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Streams;

/**
 * A {@code DataRetriever} used to read data from a csv-file.
 * 
 * @author pmeisen
 * 
 */
public class CsvDataRetriever extends BaseDataRetriever {
	private final boolean classpath;
	private final String location;
	private final String encoding;
	private final String separator;

	/**
	 * Default constructor of a {@code CsvDataRetriever} instance.
	 * 
	 * @param id
	 *            the identifier of the retriever
	 * @param c
	 *            the configuration
	 */
	public CsvDataRetriever(final String id, final IDataRetrieverConfig c) {
		super(id, c);

		final CsvDataConfig config = getConfig();

		// make sure a file is defined
		if (config.getFile() == null) {
			throw new ForwardedRuntimeException(
					CsvDataRetrieverException.class, 1007, config.getFile());
		}

		// get the InputStream
		InputStream csv = null;
		if (config.isClasspath()) {
			csv = getClass().getResourceAsStream(config.getFile());

			if (csv == null) {
				throw new ForwardedRuntimeException(
						CsvDataRetrieverException.class, 1005, config.getFile());
			}
		} else {
			final File csvFile = new File(config.getFile());

			if (!csvFile.exists()) {
				throw new ForwardedRuntimeException(
						CsvDataRetrieverException.class, 1001, config.getFile());
			} else if (!csvFile.isFile()) {
				throw new ForwardedRuntimeException(
						CsvDataRetrieverException.class, 1002, config.getFile());
			} else if (!csvFile.canRead()) {
				throw new ForwardedRuntimeException(
						CsvDataRetrieverException.class, 1003, config.getFile());
			} else {
				try {
					csv = new FileInputStream(csvFile);
				} catch (final FileNotFoundException e) {
					throw new ForwardedRuntimeException(
							CsvDataRetrieverException.class, 1004, e,
							config.getFile());
				}
			}
		}

		// set the values
		try {
			this.encoding = Streams.guessEncoding(csv, null);
		} catch (final IOException e) {
			throw new ForwardedRuntimeException(
					CsvDataRetrieverException.class, 1003, e, config.getFile());
		} finally {
			Streams.closeIO(csv);
		}

		this.location = config.getFile();
		this.classpath = config.isClasspath();
		this.separator = config.getSeparator();
	}

	/**
	 * Gets an instance of a {@code BufferedReader} used to read the content of
	 * the csv-file.
	 * 
	 * @return a {@code BufferedReader} used to read the csv-file
	 */
	protected BufferedReader getReader() {
		InputStream is;
		if (classpath) {
			is = getClass().getResourceAsStream(location);
		} else {
			try {
				is = new FileInputStream(location);
			} catch (final FileNotFoundException e) {
				exceptionRegistry.throwRuntimeException(
						CsvDataRetrieverException.class, 1004, e, location);
				return null;
			}
		}

		try {
			return new BufferedReader(new InputStreamReader(is, encoding));
		} catch (final UnsupportedEncodingException e) {
			exceptionRegistry.throwRuntimeException(
					CsvDataRetrieverException.class, 1006, e, encoding);
			return null;
		}
	}

	@Override
	public DataCollection<?> retrieve(final IQueryConfiguration config) {
		if (config == null || config instanceof CsvDataSelector) {
			// do nothing everything is fine
		} else {
			exceptionRegistry.throwRuntimeException(
					CsvDataRetrieverException.class, 1000,
					CsvDataSelector.class.getSimpleName());
		}

		try {
			return new CsvDataCollection(getReader(), separator,
					(CsvDataSelector) config);
		} catch (final ForwardedRuntimeException e) {
			exceptionRegistry.throwRuntimeException(e);
			return null;
		}
	}

	@Override
	protected boolean needConfiguration() {
		return true;
	}

	@Override
	protected Class<? extends IDataRetrieverConfig> supportedConfiguration() {
		return CsvDataConfig.class;
	}
}
