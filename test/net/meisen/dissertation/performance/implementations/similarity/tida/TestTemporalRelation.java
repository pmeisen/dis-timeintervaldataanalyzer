package net.meisen.dissertation.performance.implementations.similarity.tida;

import static org.junit.Assert.assertEquals;
import net.meisen.dissertation.performance.implementations.similarity.tida.TemporalRelation.EndEndRelation;
import net.meisen.dissertation.performance.implementations.similarity.tida.TemporalRelation.EndStartRelation;
import net.meisen.dissertation.performance.implementations.similarity.tida.TemporalRelation.Relation;
import net.meisen.dissertation.performance.implementations.similarity.tida.TemporalRelation.StartStartRelation;

import org.junit.Test;

/**
 * Tests the implementation of the {@code TemporalRelation}.
 * 
 * @author pmeisen
 * 
 */
public class TestTemporalRelation {

	/**
	 * Tests the implementation of
	 * {@code TemporalRelation#getTemporalRelation()} depending on the set
	 * start-, end- and start-end-relation.
	 */
	@Test
	public void testTemporalRelation() {
		final TemporalRelation rel = new TemporalRelation(0, 1);
		assertEquals(Relation.UNKNOWN, rel.getTemporalRelation());

		// A - STARTSWITH
		rel.setStartStartRelation(0, 1, StartStartRelation.STARTSWITH);

		// A.1 - ENDSWITH
		rel.setEndEndRelation(0, 1, EndEndRelation.ENDSWITH);

		// A.1.I
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSWITHSTART);
		assertEquals(Relation.INVALID, rel.getTemporalRelation());
		// A.1.II
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSBEFORESTART);
		assertEquals(Relation.INVALID, rel.getTemporalRelation());
		// A.1.III
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSAFTERSTART);
		assertEquals(Relation.COTEMPORAL, rel.getTemporalRelation());

		// A.2 - ENDSBEFORE
		rel.setEndEndRelation(0, 1, EndEndRelation.ENDSBEFORE);

		// A.2.I
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSWITHSTART);
		assertEquals(Relation.INVALID, rel.getTemporalRelation());
		// A.2.II
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSBEFORESTART);
		assertEquals(Relation.INVALID, rel.getTemporalRelation());
		// A.2.III
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSAFTERSTART);
		assertEquals(Relation.STARTEDBY, rel.getTemporalRelation());

		// A.3 - ENDSAFTER
		rel.setEndEndRelation(0, 1, EndEndRelation.ENDSAFTER);

		// A.3.I
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSWITHSTART);
		assertEquals(Relation.INVALID, rel.getTemporalRelation());
		// A.3.II
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSBEFORESTART);
		assertEquals(Relation.INVALID, rel.getTemporalRelation());
		// A.3.III
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSAFTERSTART);
		assertEquals(Relation.STARTEDBY, rel.getTemporalRelation());

		// B - STARTSBEFORE
		rel.setStartStartRelation(0, 1, StartStartRelation.STARTSBEFORE);

		// B.1 - ENDSWITH
		rel.setEndEndRelation(0, 1, EndEndRelation.ENDSWITH);

		// B.1.I
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSWITHSTART);
		assertEquals(Relation.INVALID, rel.getTemporalRelation());
		// B.1.II
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSBEFORESTART);
		assertEquals(Relation.INVALID, rel.getTemporalRelation());
		// B.1.III
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSAFTERSTART);
		assertEquals(Relation.FINISHEDBY, rel.getTemporalRelation());

		// B.2 - ENDSBEFORE
		rel.setEndEndRelation(0, 1, EndEndRelation.ENDSBEFORE);

		// B.2.I
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSWITHSTART);
		assertEquals(Relation.MEETS, rel.getTemporalRelation());
		// B.2.II
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSBEFORESTART);
		assertEquals(Relation.BEFORE, rel.getTemporalRelation());
		// B.2.III
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSAFTERSTART);
		assertEquals(Relation.OVERLAPS, rel.getTemporalRelation());

		// B.3 - ENDSAFTER
		rel.setEndEndRelation(0, 1, EndEndRelation.ENDSAFTER);

		// B.3.I
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSWITHSTART);
		assertEquals(Relation.INVALID, rel.getTemporalRelation());
		// B.3.II
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSBEFORESTART);
		assertEquals(Relation.INVALID, rel.getTemporalRelation());
		// B.3.III
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSAFTERSTART);
		assertEquals(Relation.CONTAINS, rel.getTemporalRelation());

		// C - STARTSAFTER
		rel.setStartStartRelation(0, 1, StartStartRelation.STARTSAFTER);

		// C.1 - ENDSWITH
		rel.setEndEndRelation(0, 1, EndEndRelation.ENDSWITH);

		// C.1.I
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSWITHSTART);
		assertEquals(Relation.INVALID, rel.getTemporalRelation());
		// C.1.II
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSBEFORESTART);
		assertEquals(Relation.INVALID, rel.getTemporalRelation());
		// C.1.III
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSAFTERSTART);
		assertEquals(Relation.INVALID, rel.getTemporalRelation());

		// C.2 - ENDSBEFORE
		rel.setEndEndRelation(0, 1, EndEndRelation.ENDSBEFORE);

		// C.2.I
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSWITHSTART);
		assertEquals(Relation.INVALID, rel.getTemporalRelation());
		// C.2.II
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSBEFORESTART);
		assertEquals(Relation.INVALID, rel.getTemporalRelation());
		// C.2.III
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSAFTERSTART);
		assertEquals(Relation.INVALID, rel.getTemporalRelation());

		// C.3 - ENDSAFTER
		rel.setEndEndRelation(0, 1, EndEndRelation.ENDSAFTER);

		// C.3.I
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSWITHSTART);
		assertEquals(Relation.INVALID, rel.getTemporalRelation());
		// C.3.II
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSBEFORESTART);
		assertEquals(Relation.INVALID, rel.getTemporalRelation());
		// C.3.III
		rel.setEndStartRelation(0, 1, EndStartRelation.ENDSAFTERSTART);
		assertEquals(Relation.INVALID, rel.getTemporalRelation());
	}

	@Test
	public void testSwapTemporalRelation() {
		final TemporalRelation rel = new TemporalRelation(0, 1);
		assertEquals(Relation.UNKNOWN, rel.getTemporalRelation());

		rel.setStartStartRelation(1, 0, StartStartRelation.STARTSAFTER);
		assertEquals(StartStartRelation.STARTSBEFORE,
				rel.getStartStartRelation());

		rel.setStartStartRelation(1, 0, StartStartRelation.STARTSBEFORE);
		assertEquals(StartStartRelation.STARTSAFTER,
				rel.getStartStartRelation());

		rel.setEndEndRelation(1, 0, EndEndRelation.ENDSAFTER);
		assertEquals(EndEndRelation.ENDSBEFORE, rel.getEndEndRelation());

		rel.setEndEndRelation(1, 0, EndEndRelation.ENDSBEFORE);
		assertEquals(EndEndRelation.ENDSAFTER, rel.getEndEndRelation());

		rel.setEndStartRelation(1, 0, EndStartRelation.ENDSAFTERSTART);
		assertEquals(EndStartRelation.UNKNOWN, rel.getEndStartRelation());

		rel.setEndStartRelation(1, 0, EndStartRelation.ENDSBEFORESTART);
		assertEquals(EndStartRelation.UNKNOWN, rel.getEndStartRelation());
	}
}
