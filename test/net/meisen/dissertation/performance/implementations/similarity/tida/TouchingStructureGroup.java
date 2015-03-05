package net.meisen.dissertation.performance.implementations.similarity.tida;

import java.util.Arrays;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.performance.implementations.similarity.tida.TemporalRelation.EndEndRelation;
import net.meisen.dissertation.performance.implementations.similarity.tida.TemporalRelation.EndStartRelation;
import net.meisen.dissertation.performance.implementations.similarity.tida.TemporalRelation.StartStartRelation;

public class TouchingStructureGroup extends StructureGroup {
	private final static Logger LOG = LoggerFactory
			.getLogger(TouchingStructureGroup.class);

	private final static int CONTAINER_SIZE = 1;
	private final static int POS_TP_BMP = 0;

	public TouchingStructureGroup(final int maxPosition) {
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

		// keep the values
		if (nCntr != null) {
			nCntr[POS_TP_BMP] = bitmap;
		}

		// log the started and ended values
		if (LOG.isTraceEnabled()) {
			LOG.trace("    Running    : " + bmpNowRun);
			LOG.trace("    Started    : " + bmpNowStart);
			LOG.trace("    Ended      : " + bmpNowEnd);
		}

		// use the result to determine temporal-relations
		setStartRelation(bmpNowStart, bmpNowEnd, bmpNowRun);
		setEndRelation(bmpNowEnd, bmpNowRun, bmpNowStart);

		// clean the results and write those as values
		return cleanResults(bmpPre, pos);
	}

	protected void setStartRelation(final Bitmap bmpNowStart,
			final Bitmap bmpNowEnd, final Bitmap bmpNowRun) {
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

			if (bmpNowRun != null) {
				for (final int k : bmpNowRun) {
					set(k, id, StartStartRelation.STARTSBEFORE,
							EndStartRelation.ENDSAFTERSTART, null, null);
				}
			}

			if (bmpNowEnd != null) {
				for (final int k : bmpNowEnd) {
					set(k, id, StartStartRelation.STARTSBEFORE,
							EndStartRelation.ENDSWITHSTART,
							EndEndRelation.ENDSBEFORE,
							EndStartRelation.ENDSWITHSTART);
				}
			}
		}
	}

	protected void setEndRelation(final Bitmap bmpNowEnd,
			final Bitmap bmpNowRun, final Bitmap bmpNowStart) {
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

			if (bmpNowStart != null) {
				for (final int k : bmpNowStart) {
					set(id, k, null, EndStartRelation.ENDSWITHSTART, null, null);
				}
			}
		}
	}

}
