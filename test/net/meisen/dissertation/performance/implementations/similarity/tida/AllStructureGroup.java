package net.meisen.dissertation.performance.implementations.similarity.tida;

import java.util.Arrays;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.performance.implementations.similarity.tida.TemporalRelation.EndEndRelation;
import net.meisen.dissertation.performance.implementations.similarity.tida.TemporalRelation.EndStartRelation;
import net.meisen.dissertation.performance.implementations.similarity.tida.TemporalRelation.StartStartRelation;

public class AllStructureGroup extends StructureGroup {
	private final static Logger LOG = LoggerFactory
			.getLogger(AllStructureGroup.class);

	private final static int CONTAINER_SIZE = 3;
	private final static int POS_TP_BMP = 0;
	private final static int POS_STARTED_BMP = 1;
	private final static int POS_ENDED_BMP = 2;

	public AllStructureGroup(final int maxPosition) {
		super(maxPosition);
	}

	@Override
	public Bitmap[] createContainer() {
		return new Bitmap[CONTAINER_SIZE];
	}

	@Override
	protected int[] calc(final int pos, final Bitmap bitmap,
			final Bitmap[] oCntr, final Bitmap[] nCntr) {

		if (getMaxPosition() < pos) {
			// TODO make it nice
			throw new IllegalStateException();
		}

		// log the calculation
		if (LOG.isTraceEnabled()) {
			LOG.trace("Calculating structure at " + pos + ":");
			LOG.trace(oCntr == null ? null : Arrays.asList(oCntr).toString());
			LOG.trace(nCntr == null ? null : Arrays.asList(nCntr).toString());
		}

		// get the previous and now bitmap
		final Bitmap bmpPre = oCntr == null ? null : oCntr[POS_TP_BMP];
		final Bitmap bmpNow = bitmap == null ? null : bitmap;

		// get the now started and ended
		final Bitmap bmpNowStart, bmpNowEnd, bmpNowRun;
		if (bmpPre == null || bmpNow == null) {
			bmpNowStart = bmpNow;
			bmpNowEnd = bmpPre;
			bmpNowRun = null;
		} else {
			final Bitmap bmpTmp = bmpPre.xor(bmpNow);
			bmpNowStart = bmpTmp.and(bmpNow);
			bmpNowEnd = bmpTmp.and(bmpPre);
			bmpNowRun = bmpPre.and(bmpNow);
		}

		// determine the overall started
		final Bitmap bmpPreAllStarted = oCntr == null ? null
				: oCntr[POS_STARTED_BMP];
		final Bitmap bmpAllStarted;
		if (bmpPreAllStarted == null) {
			bmpAllStarted = bmpNowStart;
		} else {
			bmpAllStarted = bmpPreAllStarted.or(bmpNowStart);
		}

		// determine the overall ended
		final Bitmap bmpPreAllEnded = oCntr == null ? null
				: oCntr[POS_ENDED_BMP];
		final Bitmap bmpAllEnded;
		if (bmpPreAllEnded == null) {
			bmpAllEnded = bmpNowEnd;
		} else {
			bmpAllEnded = bmpPreAllEnded.or(bmpNowEnd);
		}

		// keep the values
		if (nCntr != null) {
			nCntr[POS_TP_BMP] = bitmap;
			nCntr[POS_STARTED_BMP] = bmpAllStarted;
			nCntr[POS_ENDED_BMP] = bmpAllEnded;
		}

		// log the started and ended values
		if (LOG.isTraceEnabled()) {
			LOG.trace("    Running    : " + bmpNowRun);
			LOG.trace("    Started    : " + bmpNowStart);
			LOG.trace("    Ended      : " + bmpNowEnd);
			LOG.trace("Pre All Started: " + bmpPreAllStarted);
			LOG.trace("Pre All Ended  : " + bmpPreAllEnded);
			LOG.trace("    All Started: " + bmpAllStarted);
			LOG.trace("    All Ended  : " + bmpAllEnded);
		}

		// use the result to determine temporal-relations
		setStartRelation(bmpNowStart, bmpNowRun, bmpPreAllStarted,
				bmpPreAllEnded);
		setEndRelation(bmpNowEnd, bmpNowRun, bmpPreAllEnded, bmpNowStart);

		// clean the results and write those as values
		return cleanResults(bmpPre, pos);
	}

	protected void setStartRelation(final Bitmap bmpNowStart,
			final Bitmap bmpNowRun, final Bitmap bmpPreAllStarted,
			final Bitmap bmpPreAllEnded) {
		if (bmpNowStart == null || !bmpNowStart.isBitSet()) {
			return;
		}

		final int[] ids = bmpNowStart.getIds();
		final int length = ids.length;
		for (int i = 0; i < length; i++) {
			final int id = ids[i];

			for (int k = i + 1; k < length; k++) {
				set(id, ids[k], StartStartRelation.STARTSWITH,
						EndStartRelation.ENDSAFTERSTART, null, null);
			}

			if (bmpPreAllStarted != null) {
				for (final int k : bmpPreAllStarted) {
					set(k, id, StartStartRelation.STARTSBEFORE, null, null,
							null);
				}
			}

			if (bmpNowRun != null) {
				for (final int k : bmpNowRun) {
					set(k, id, StartStartRelation.STARTSBEFORE,
							EndStartRelation.ENDSAFTERSTART, null, null);
				}
			}

			if (bmpPreAllEnded != null) {
				for (final int k : bmpPreAllEnded) {
					set(k, id, null, EndStartRelation.ENDSBEFORESTART, null,
							null);
				}
			}
		}
	}

	protected void setEndRelation(final Bitmap bmpNowEnd,
			final Bitmap bmpNowRun, final Bitmap bmpPreAllEnded,
			final Bitmap bmpNowStart) {
		if (bmpNowEnd == null || !bmpNowEnd.isBitSet()) {
			return;
		}

		final int[] ids = bmpNowEnd.getIds();
		final int length = ids.length;
		for (int i = 0; i < length; i++) {
			final int id = ids[i];

			for (int k = i + 1; k < length; k++) {
				set(id, ids[k], null, EndStartRelation.ENDSAFTERSTART,
						EndEndRelation.ENDSWITH,
						EndStartRelation.ENDSAFTERSTART);
			}

			if (bmpNowRun != null) {
				for (final int k : bmpNowRun) {
					set(id, k, null, EndStartRelation.ENDSAFTERSTART,
							EndEndRelation.ENDSBEFORE,
							EndStartRelation.ENDSAFTERSTART);
				}
			}

			if (bmpPreAllEnded != null) {
				for (final int k : bmpPreAllEnded) {
					set(id, k, null, null, EndEndRelation.ENDSAFTER, null);
				}
			}

			if (bmpNowStart != null) {
				for (final int k : bmpNowStart) {
					set(id, k, null, EndStartRelation.ENDSWITHSTART, null, null);
				}
			}
		}
	}
}
