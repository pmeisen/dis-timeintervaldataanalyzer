package net.meisen.dissertation.model.auth.permissions;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * Tests the {@code DefinedPermission}.
 * 
 * @author pmeisen
 * 
 */
public class TestDefinedPermission {

	/**
	 * Tests the {@link DefinedPermission#toString(DefinedPermission[][])}
	 * implementation.
	 */
	@Test
	public void testToString() {
		DefinedPermission[][] permSets;

		permSets = null;
		assertEquals("", DefinedPermission.toString(permSets));

		permSets = new DefinedPermission[][] {};
		assertEquals("", DefinedPermission.toString(permSets));

		permSets = new DefinedPermission[][] { new DefinedPermission[] {} };
		assertEquals("()", DefinedPermission.toString(permSets));

		permSets = new DefinedPermission[][] { new DefinedPermission[] { new DefinedPermission(
				Permission.get, null) } };
		assertEquals("(GLOBAL.get)", DefinedPermission.toString(permSets));

		permSets = new DefinedPermission[][] { new DefinedPermission[] { new DefinedPermission(
				Permission.get, null) } };
		assertEquals("(GLOBAL.get)", DefinedPermission.toString(permSets));

		permSets = new DefinedPermission[][] { new DefinedPermission[] {
				new DefinedPermission(Permission.get, null),
				new DefinedPermission(Permission.load, null) } };
		assertEquals("(GLOBAL.get, GLOBAL.load)",
				DefinedPermission.toString(permSets));

		permSets = new DefinedPermission[][] {
				new DefinedPermission[] {
						new DefinedPermission(Permission.get, null),
						new DefinedPermission(Permission.load, null) },
				new DefinedPermission[] { new DefinedPermission(
						Permission.query, "MyModel") } };
		assertEquals("(GLOBAL.get, GLOBAL.load), (MODEL.MyModel.query)",
				DefinedPermission.toString(permSets));
	}

	/**
	 * Tests the {@link DefinedPermission#fromString(String, String)}
	 * implementation.
	 */
	@Test
	public void testFromString() {
		boolean exception;
		DefinedPermission perm = null;

		// check some invalid strings
		try {
			exception = false;
			perm = DefinedPermission.fromString("", "/");
		} catch (final Exception e) {
			exception = true;
		}
		assertTrue(exception);

		try {
			exception = false;
			perm = DefinedPermission.fromString("a/not/valid/one", "/");
		} catch (final Exception e) {
			exception = true;
		}
		assertTrue(exception);

		try {
			exception = false;
			perm = DefinedPermission.fromString("a/not/valid", "/");
		} catch (final Exception e) {
			exception = true;
		}
		assertTrue(exception);

		try {
			exception = false;
			perm = DefinedPermission.fromString("GLOBAL.getter", ".");
		} catch (final Exception e) {
			exception = true;
		}
		assertTrue(exception);

		try {
			exception = false;
			perm = DefinedPermission.fromString("GLOBAL.myModel.getter", ".");
		} catch (final Exception e) {
			exception = true;
		}
		assertTrue(exception);

		try {
			exception = false;
			perm = DefinedPermission.fromString("MODEL.query", ".");
		} catch (final Exception e) {
			exception = true;
		}
		assertTrue(exception);

		// check some valid once
		perm = DefinedPermission.fromString("GLOBAL.get", ".");
		assertEquals(Permission.get, perm.getPermission());
		assertNull(perm.getModelId());

		perm = DefinedPermission.fromString("MODEL.myModel.query", ".");
		assertEquals(Permission.query, perm.getPermission());
		assertEquals("myModel", perm.getModelId());
	}
}
