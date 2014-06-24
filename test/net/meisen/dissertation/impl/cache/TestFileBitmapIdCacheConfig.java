package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;

/**
 * Tests the implementation of a {@code FileBitmapCacheConfig}
 * 
 * @author pmeisen
 * 
 */
public class TestFileBitmapIdCacheConfig {

	/**
	 * Tests the calculation of the configured max file-size.
	 */
	@Test
	public void testMaxFileSize() {
		final FileBitmapIdCacheConfig c = new FileBitmapIdCacheConfig();

		// check default value
		assertNull(c.getMaxFileSizeInByte());

		// check megabyte (and default usage)
		c.setMaxFileSize("10000b");
		assertEquals(new Integer(10000), c.getMaxFileSizeInByte());
		c.setMaxFileSize("10000B");
		assertEquals(new Integer(10000), c.getMaxFileSizeInByte());

		// check megabyte (and default usage)
		c.setMaxFileSize("1024");
		assertEquals(new Integer(1073741824), c.getMaxFileSizeInByte());
		c.setMaxFileSize("1024M");
		assertEquals(new Integer(1073741824), c.getMaxFileSizeInByte());
		c.setMaxFileSize("1024m");
		assertEquals(new Integer(1073741824), c.getMaxFileSizeInByte());
		c.setMaxFileSize("   1024  m   ");
		assertEquals(new Integer(1073741824), c.getMaxFileSizeInByte());

		// check kilobyte
		c.setMaxFileSize("1k");
		assertEquals(new Integer(1024), c.getMaxFileSizeInByte());
		c.setMaxFileSize("1K");
		assertEquals(new Integer(1024), c.getMaxFileSizeInByte());

		// check gigabyte
		c.setMaxFileSize("1.5g");
		assertEquals(new Integer(1610612736), c.getMaxFileSizeInByte());
		c.setMaxFileSize("1.5g");
		assertEquals(new Integer(1610612736), c.getMaxFileSizeInByte());
		c.setMaxFileSize(".5g");
		assertEquals(new Integer(536870912), c.getMaxFileSizeInByte());

		// check some special values/situations
		c.setMaxFileSize("m");
		assertEquals(new Integer(1048576), c.getMaxFileSizeInByte());
		c.setMaxFileSize("1000g");
		assertEquals(new Integer(Integer.MAX_VALUE), c.getMaxFileSizeInByte());

		// check some invalid values
		c.setMaxFileSize(null);
		assertNull(c.getMaxFileSizeInByte());
		c.setMaxFileSize("");
		assertNull(c.getMaxFileSizeInByte());
		c.setMaxFileSize("1,5");
		assertNull(c.getMaxFileSizeInByte());
		c.setMaxFileSize("20%");
		assertNull(c.getMaxFileSizeInByte());
		c.setMaxFileSize("1000mk");
		assertNull(c.getMaxFileSizeInByte());
	}
}
