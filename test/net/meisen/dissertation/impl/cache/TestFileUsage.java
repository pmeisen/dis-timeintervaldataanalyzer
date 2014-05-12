package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.googlecode.javaewah.EWAHCompressedBitmap;

public class TestFileUsage {

	private int[] huge;

	private File file;
	private DataOutputStream dos;
	private RandomAccessFile dis;

	private List<Integer> positions = Collections
			.synchronizedList(new ArrayList<Integer>());

	@Before
	public void init() throws IOException {

		huge = new int[10000];
		for (int i = 0; i < 20000; i++) {
			if (i % 2 == 0)
				huge[(int) (0.5 * i)] = i;
		}

		file = File.createTempFile(UUID.randomUUID().toString(), ".tmp");
		dos = new DataOutputStream(new FileOutputStream(file));
		dis = new RandomAccessFile(file, "r");
	}

	private int[] writeBitmap(int... nr) throws IOException {
		EWAHCompressedBitmap bmp;
		bmp = EWAHCompressedBitmap.bitmapOf(nr);
		
		synchronized (dos) {
			int priorSize = dos.size();

			bmp.serialize(dos);
			dos.flush();

			int afterSize = dos.size();
			return new int[] { afterSize - priorSize, priorSize };
		}
	}

	private EWAHCompressedBitmap readBitmap(final int pos) throws IOException {

		EWAHCompressedBitmap bmp;
		bmp = EWAHCompressedBitmap.bitmapOf();

		synchronized (dis) {
			dis.seek(pos);
			bmp.deserialize(dis);
		}

		return bmp;
	}

	@Test
	public void test() throws InterruptedException {

		final int maxWriterThreads = 1;
		final int maxBitmapPerThread = 10000;

		// a writer thread
		final Thread[] writer = new Thread[maxWriterThreads];
		for (int i = 0; i < maxWriterThreads; i++) {
			final int nr = i;

			writer[i] = new Thread() {

				@Override
				public void run() {
					try {
						for (int k = 0; k < maxBitmapPerThread; k++) {
							int[] res = writeBitmap(huge);
							positions.add(res[1]);
						}
					} catch (final Exception e) {
						e.printStackTrace();
					}
				}
			};
		}

		// a reader thread
		final int maxReaderThreads = 1000;
		final Thread[] reader = new Thread[maxReaderThreads];
		for (int i = 0; i < maxReaderThreads; i++) {
			reader[i] = new Thread() {

				@Override
				public void run() {
					try {
						int pos = positions.size() - 1;
						if (pos != -1) {
							final EWAHCompressedBitmap bmp = readBitmap(positions
									.get(pos));
							assertEquals("" + positions.get(pos), huge.length,
									bmp.cardinality());
						}
					} catch (final IOException e) {
						e.printStackTrace();
					}
				}
			};
		}

		for (int i = 0; i < maxWriterThreads; i++) {
			writer[i].start();
		}
		for (int i = 0; i < maxReaderThreads; i++) {
			reader[i].start();
		}

		for (int i = 0; i < maxWriterThreads; i++) {
			writer[i].join();
		}
		for (int i = 0; i < maxReaderThreads; i++) {
			reader[i].join();
		}
	}

	@After
	public void cleanUp() throws IOException {
		dos.close();
		dis.close();
		assertTrue(file.delete());
	}
}
