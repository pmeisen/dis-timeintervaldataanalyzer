package net.meisen.dissertation.performance.implementations.similarity.tida;

import gnu.trove.iterator.TIntObjectIterator;
import gnu.trove.map.hash.TIntObjectHashMap;
import gnu.trove.map.hash.TLongObjectHashMap;

import java.util.Iterator;
import java.util.Locale;

import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.performance.implementations.similarity.tida.TemporalRelation.EndEndRelation;
import net.meisen.dissertation.performance.implementations.similarity.tida.TemporalRelation.EndStartRelation;
import net.meisen.dissertation.performance.implementations.similarity.tida.TemporalRelation.Relation;
import net.meisen.dissertation.performance.implementations.similarity.tida.TemporalRelation.StartStartRelation;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class StructureGroup {
	private final static Logger LOG = LoggerFactory
			.getLogger(StructureGroup.class);

	private final TIntObjectHashMap<int[]> strucResults;
	private final TLongObjectHashMap<TemporalRelation> results;
	private final TIntObjectHashMap<Bitmap[]> lastBitmaps;

	private final int maxPosition;

	public StructureGroup(final int maxPosition) {
		this.strucResults = new TIntObjectHashMap<int[]>();
		this.results = new TLongObjectHashMap<TemporalRelation>();
		this.lastBitmaps = new TIntObjectHashMap<Bitmap[]>(1, 1.0f);

		this.maxPosition = maxPosition;
	}

	public void set(final int recId1, final int recId2,
			final StartStartRelation startStartRelation) {
		set(recId1, recId2, startStartRelation, null, null, null);
	}

	public void set(final int recId1, final int recId2,
			final EndStartRelation endStartRelation) {
		set(recId1, recId2, null, endStartRelation, null, null);
	}

	public void set(final int recId1, final int recId2,
			final EndEndRelation endEndRelation) {
		set(recId1, recId2, null, null, endEndRelation, null);
	}

	public void set(final int recId1, final int recId2,
			final StartStartRelation startStartRelation,
			final EndStartRelation endStartRelation,
			final EndEndRelation endEndRelation,
			final EndStartRelation swappedEndStartRelation) {
		final long pos = getUniquePos(recId1, recId2);

		TemporalRelation rel = results.get(pos);
		if (rel == null) {
			// make sure it's a new one and not an old information
			if (startStartRelation == null) {
				return;
			}

			rel = new TemporalRelation(recId1, recId2);
			results.put(pos, rel);
		}
		rel.set(recId1, recId2, startStartRelation, endStartRelation,
				endEndRelation, swappedEndStartRelation);
	}

	/**
	 * Retrieves the last (considering the maxPosition) container used. If no
	 * container is available for the previous maxPosition, the method returns
	 * an empty container.
	 * 
	 * @param pos
	 *            the position (typically timepoint + 1)
	 * @param container
	 *            the container to be used to store data
	 * 
	 * @return returns the old container associated to the previous maxPosition
	 *         (i.e. pos - 1), if no container was associated an empty container
	 *         (i.e. all values {@code null}) are returned; thus never returns
	 *         {@code null}
	 */
	protected Bitmap[] retrieveAndSet(final int pos, final Bitmap[] container) {
		final Bitmap[] lastBitmap = lastBitmaps.get(pos - 1);

		/*
		 * Make sure to handleLast if there is one.
		 */
		if (lastBitmap == null) {
			handleLastBitmap();
		}

		// add the new last
		lastBitmaps.put(pos, container);

		return lastBitmap == null ? null : lastBitmap;
	}

	public abstract Bitmap[] createContainer();

	protected abstract int[] calc(final int pos, final Bitmap bitmap,
			final Bitmap[] oCntr, final Bitmap[] nCntr);

	public void handleLastBitmap() {

		if (!lastBitmaps.isEmpty()) {
			final TIntObjectIterator<Bitmap[]> it = lastBitmaps.iterator();
			it.advance();

			calc(it.key() + 1, null, it.value(), null);

			// remove everything not needed anymore
			lastBitmaps.clear();
		}
	}

	public int[] calc(final int pos, final Bitmap bitmap) {

		// create the new container and swap it with the old one
		final Bitmap[] nCntr = createContainer();
		final Bitmap[] oCntr = retrieveAndSet(pos, nCntr);

		return calc(pos, bitmap, oCntr, nCntr);
	}

	protected int[] cleanResults(final Bitmap bmpPre, final int pos) {
		final Iterator<TemporalRelation> it = results.valueCollection()
				.iterator();

		int[] container = null;
		while (it.hasNext()) {
			final TemporalRelation rel = it.next();
			if (!rel.isUnknown()) {
				final Relation tRel = rel.getTemporalRelation();

				if (!tRel.isFinal()) {
					// TODO; make nice
					throw new IllegalStateException("tRel");
				} else {
					if (LOG.isTraceEnabled()) {
						LOG.trace("Result: " + rel);
					}

					// make sure we have a container
					if (container == null) {
						container = getOrCreate(pos);
					}

					// increase the value
					container[tRel.ordinal() + 1]++;
					it.remove();
				}
			}
		}

		// set the count of values
		if (container != null) {
			container[0] = bmpPre == null ? 0 : bmpPre.determineCardinality();
		}

		return container;
	}

	protected int[] getOrCreate(final int pos) {
		int[] strucResult = get(pos);
		if (strucResult == null) {
			strucResult = new int[Relation.values().length + 1];
			strucResults.put(pos, strucResult);
		}

		return strucResult;
	}

	public int[] get(final int pos) {
		return strucResults.get(pos);
	}

	/**
	 * Uses a pairing-function to determine a unique identifier for the
	 * identifiers of the two-records. The identifier is equal for the two pairs
	 * {@code (recId1, recId2)} and {@code (recId2, recId1)}. This behavior is
	 * required, because the order is not known, nevertheless the retrieval
	 * ensures the correct retrieval of the {@code TemporalRelation} instance.
	 * 
	 * @param recId1
	 *            the identifier of the first record
	 * @param recId2
	 *            the identifier of the second record
	 * 
	 * @return the unique identifier to be used
	 */
	protected long getUniquePos(final int recId1, final int recId2) {
		final long x = Math.max(recId1, recId2);
		final long y = Math.min(recId1, recId2);

		return (long) (0.5 * (x * (x + 1)) + y);
	}

	@Override
	public String toString() {
		return toString(true);
	}

	public String toString(final boolean shortVersion) {
		final StringBuilder sb = new StringBuilder();

		int added = 0;
		for (int i = 1; i <= getMaxPosition(); i++) {
			final String vals = toString(get(i), shortVersion);
			if (!shortVersion || !vals.isEmpty()) {
				if (added > 0) {
					sb.append(", ");
				}

				sb.append(String.format(Locale.US, "[%s] (%d)", vals, i));
				added++;
			}
		}

		return sb.toString().trim();
	}

	protected String toString(int[] vals, final boolean shortVersion) {
		final StringBuilder sb = new StringBuilder();

		final Relation[] rels = Relation.values();
		final int len = Relation.values().length + 1;
		if (vals == null) {
			vals = new int[len];
		}

		int added = 0;
		for (int i = 0; i < len; i++) {
			if (vals[i] > 0) {
				if (added > 0) {
					sb.append(", ");
				}

				if (i == 0) {
					sb.append("COUNT (" + vals[i] + ")");
				} else {
					sb.append(rels[i - 1].name() + " (" + vals[i] + ")");
				}
				added++;
			}
		}

		return sb.toString().trim();
	}

	public int getMaxPosition() {
		return maxPosition;
	}
}