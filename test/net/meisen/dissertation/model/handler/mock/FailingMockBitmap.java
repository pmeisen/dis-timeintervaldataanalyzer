package net.meisen.dissertation.model.handler.mock;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;

import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.util.IIntIterator;

/**
 * A bitmap mock which fails during initialization.
 * 
 * @author pmeisen
 * 
 */
public class FailingMockBitmap extends Bitmap {

	private FailingMockBitmap() {

	}

	@Override
	public int[] getIds() {
		return null;
	}

	@Override
	public int getMaxId() {
		return Integer.MAX_VALUE;
	}

	@Override
	public int getMinId() {
		return 0;
	}

	@Override
	public int determineCardinality() {
		return 0;
	}

	@Override
	public boolean isBitSet() {
		return false;
	}

	@Override
	public void optimize() {
		// nothing to do
	}

	@Override
	protected void set(int... recordIds) {
		// nothing to do
	}

	@Override
	public Bitmap copy() {
		return this;
	}

	@Override
	public void serialize(DataOutput out) throws IOException {
		// nothing to do
	}

	@Override
	protected void deserialize(DataInput in) throws IOException {
		// nothing to do
	}

	@Override
	public Bitmap invert(int position) {
		return this;
	}

	@Override
	public int invertCardinality(int position) {
		return 0;
	}

	@Override
	public Bitmap and(Bitmap... bitmaps) {
		return this;
	}

	@Override
	public int andCardinality(Bitmap... bitmaps) {
		return 0;
	}

	@Override
	public Bitmap or(Bitmap... bitmaps) {
		return this;
	}

	@Override
	public Bitmap xor(Bitmap... bitmaps) {
		return this;
	}

	@Override
	public int orCardinality(Bitmap... bitmaps) {
		return 0;
	}

	@Override
	public IIntIterator intIterator() {
		return new IIntIterator() {

			@Override
			public int next() {
				return 0;
			}

			@Override
			public boolean hasNext() {
				return false;
			}
		};
	}

}
