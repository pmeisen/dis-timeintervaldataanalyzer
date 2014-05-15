package net.meisen.dissertation.impl.cache;

import java.util.Arrays;

import net.meisen.general.genmisc.types.Streams;

/**
 * An {@code IndexEntry} is an entry in the index-file (see
 * {@link FileCache#idxTableFileName}) of the cache. It defines the
 * 
 * @author pmeisen
 * 
 */
public class IndexEntry {

	/**
	 * The size of an {@code IndexEntry} as bytes.
	 */
	public static final int idxEntrySizeInBytes = 16;

	private final int size;
	private final int bitmapFilePosition;
	private final int fileNumber;

	private int indexFileNumber;

	/**
	 * Constructor to create a {@code IndexEntry} from a byte-array.
	 * 
	 * @param bytes
	 *            the array to read the {@code IndexEntry} from
	 */
	public IndexEntry(final byte[] bytes) {
		if (bytes.length != idxEntrySizeInBytes) {
			throw new IllegalArgumentException(
					"The amount of bytes to load the IndexEntry from is invalid ('"
							+ bytes.length + "' != '" + idxEntrySizeInBytes
							+ "').");
		}

		// deserialize the values
		final int size = Streams.byteToInt(Arrays.copyOfRange(bytes, 0, 4));
		final int bfp = Streams.byteToInt(Arrays.copyOfRange(bytes, 4, 8));
		final int fn = Streams.byteToInt(Arrays.copyOfRange(bytes, 8, 12));
		final int ifp = Streams.byteToInt(Arrays.copyOfRange(bytes, 12, 16));

		// set the values
		this.size = size;
		this.bitmapFilePosition = bfp;
		this.fileNumber = fn;
		this.indexFileNumber = ifp;
	}

	/**
	 * Constructor to create a not persisted entry. The {@code indexFileNumber}
	 * is set to {@code -1}.
	 * 
	 * @param size
	 *            the size of the entry in the bitmap-file (see
	 *            {@link FileCache#bitmapFileName}) in bytes
	 * @param bitmapFilePosition
	 *            the position (in bytes) of the entry within the bitmap-file
	 *            (see {@link FileCache#bitmapFileName})
	 * @param fileNumber
	 *            the number of the file the entry belongs to
	 */
	public IndexEntry(final int size, final int bitmapFilePosition,
			final int fileNumber) {
		this.size = size;
		this.bitmapFilePosition = bitmapFilePosition;
		this.fileNumber = fileNumber;
		this.indexFileNumber = -1;
	}

	/**
	 * Gets the size of the entry in the bitmap-file (see
	 * {@link FileCache#bitmapFileName}) in bytes.
	 * 
	 * @return the size of the entry in the bitmap-file (see
	 *         {@link FileCache#bitmapFileName}) in bytes
	 */
	public int getSize() {
		return size;
	}

	/**
	 * Gets the position (in bytes) of the entry within the bitmap-file (see
	 * {@link FileCache#bitmapFileName}).
	 * 
	 * @return the position (in bytes) of the entry within the bitmap-file (see
	 *         {@link FileCache#bitmapFileName})
	 */
	public int getBitmapFilePosition() {
		return bitmapFilePosition;
	}

	/**
	 * Gets the number of the {@code IndexEntry}, i.e. the zero-based position
	 * of the entry within the bitmap-file (see {@link FileCache#bitmapFileName}
	 * ).
	 * 
	 * @return the number of the {@code IndexEntry} (zero-based)
	 */
	public int getIndexFileNumber() {
		return indexFileNumber;
	}

	/**
	 * Gets the number of the file the entry belongs to.
	 * 
	 * @return the number of the file the entry belongs to
	 */
	public int getFileNumber() {
		return fileNumber;
	}

	/**
	 * Sets the number of the {@code IndexEntry} for the entry.
	 * 
	 * @param indexFileNumber
	 *            the number of the {@code IndexEntry} (zero-based)
	 */
	public void setIndexFileNumber(final int indexFileNumber) {
		this.indexFileNumber = indexFileNumber;
	}

	/**
	 * Gets the byte representation of the {@code IndexEntry}.
	 * 
	 * @return the byte representation of the {@code IndexEntry}
	 */
	public byte[] bytes() {
		final byte[] bytesSize = Streams.intToByte(getSize());
		final byte[] bytesBfp = Streams.intToByte(getBitmapFilePosition());
		final byte[] bytesFn = Streams.intToByte(getFileNumber());
		final byte[] bytesIfp = Streams.intToByte(getIndexFileNumber());

		return Streams.combineBytes(bytesSize, bytesBfp, bytesFn, bytesIfp);
	}

	@Override
	public String toString() {
		return bitmapFilePosition + ", " + size + ", " + indexFileNumber + ", "
				+ fileNumber;
	}
}