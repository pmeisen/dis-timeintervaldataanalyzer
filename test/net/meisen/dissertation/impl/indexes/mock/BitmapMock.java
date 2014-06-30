package net.meisen.dissertation.impl.indexes.mock;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;

import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.util.IIntIterator;

/**
 * A mock of a {@code Bitmap} used to test the definition of a bitmap type using
 * configuration.
 * 
 * @author pmeisen
 * 
 */
public class BitmapMock extends Bitmap {

	@Override
	public int[] getIds() {
		return null;
	}

	@Override
	public int determineCardinality() {
		return 0;
	}

	@Override
	public void optimize() {
		// nothing
	}

	@Override
	protected void set(final int... recordIds) {
		// nothing
	}

	@Override
	public void serialize(final DataOutput out) throws IOException {
		// nothing
	}

	@Override
	public void deserialize(final DataInput in) throws IOException {
		// nothing
	}

	@Override
	public Bitmap and(final Bitmap... bitmaps) {
		return null;
	}

	@Override
	public int andCardinality(final Bitmap... bitmaps) {
		return 0;
	}

	@Override
	public Bitmap or(final Bitmap... bitmaps) {
		return null;
	}
	
	@Override
	public Bitmap xor(final Bitmap... bitmaps) {
		return null;
	}

	@Override
	public int orCardinality(final Bitmap... bitmaps) {
		return 0;
	}

	@Override
	public Bitmap invert(int position) {
		return null;
	}

	@Override
	public int invertCardinality(int position) {
		return 0;
	}

	@Override
	public Bitmap copy() {
		return this;
	}

	@Override
	public int getMaxId() {
		return 0;
	}

	@Override
	public int getMinId() {
		return 0;
	}

	@Override
	public IIntIterator intIterator() {
		return null;
	}

	@Override
	public boolean isBitSet() {
		return false;
	}
}
