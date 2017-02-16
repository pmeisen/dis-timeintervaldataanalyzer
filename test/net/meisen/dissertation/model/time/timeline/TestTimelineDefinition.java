package net.meisen.dissertation.model.time.timeline;

import net.meisen.dissertation.model.time.granularity.DateFormat;
import net.meisen.dissertation.model.time.granularity.Day;
import net.meisen.dissertation.model.time.granularity.MilliSecond;
import net.meisen.dissertation.model.time.granularity.Second;
import net.meisen.dissertation.model.time.granularity.Year;
import net.meisen.dissertation.model.time.timeline.TimelineDefinition.Position;
import net.meisen.general.genmisc.types.Dates;
import org.junit.Test;

import java.util.Date;

import static org.junit.Assert.assertEquals;

/**
 * Tests the implementation of {@code TimelineDefinition}.
 *
 * @author pmeisen
 */
public class TestTimelineDefinition {

    private Date getDate(final String date) {
        return Dates.isDate(date, Dates.GENERAL_TIMEZONE);
    }

    /**
     * Tests the implementation of
     * {@link TimelineDefinition#getDef(Position, Date)}.
     */
    @Test
    public void testDefCalculation() {
        Date pit;
        int year;
        final TimelineDefinition def = new TimelineDefinition(1, 1,
                MilliSecond.instance());
        final int off = TimelineDefinition.OFFSET;

        // create the pit and check the results
        year = 1981;
        pit = getDate("20.01." + year);
        assertEquals(getDate("20.01." + (year - off)),
                def.getDef(Position.START, pit));
        assertEquals(getDate("20.01." + (year + off)),
                def.getDef(Position.END, pit));

        // create the pit and check the results
        year = 100;
        pit = getDate("20.01." + year);
        assertEquals(getDate("20.01." + (year - off)),
                def.getDef(Position.START, pit));
        assertEquals(getDate("20.01." + (year + off)),
                def.getDef(Position.END, pit));

        // create the pit and check the results
        year = 2090;
        pit = getDate("20.01." + year);
        assertEquals(getDate("20.01." + (year - off)),
                def.getDef(Position.START, pit));
        assertEquals(getDate("20.01." + (year + off)),
                def.getDef(Position.END, pit));

    }

    /**
     * Tests the usage of a integer constructor.
     */
    @Test
    public void testIntegerTimeline() {
        final TimelineDefinition def = new TimelineDefinition(-100, 5,
                MilliSecond.instance());
        assertEquals(Long.class, def.getType());
        assertEquals(Long.valueOf(-100L), def.getStart());
        assertEquals(Long.valueOf(5L), def.getEnd());
    }

    /**
     * Tests the usage of a long constructor.
     */
    @Test
    public void testLongTimeline() {
        final TimelineDefinition def = new TimelineDefinition(10L, 100L, MilliSecond.instance());
        assertEquals(Long.class, def.getType());
        assertEquals(Long.valueOf(10L), def.getStart());
        assertEquals(Long.valueOf(100L), def.getEnd());
    }

    /**
     * Tests the usage of a date constructor.
     */
    @Test
    public void testDateTimeline() {
        final Date now = new Date();
        final TimelineDefinition def = new TimelineDefinition(now, now,
                MilliSecond.instance());

        assertEquals(Date.class, def.getType());
        assertEquals(now, def.getStart());
        assertEquals(now, def.getEnd());
    }

    /**
     * Tests the usage of a date constructor with null.
     */
    @Test
    public void testDateWithNullValue() {
        final Date pit = getDate("10.10.2010 01:22:33");
        final TimelineDefinition def = new TimelineDefinition(pit, null,
                MilliSecond.instance());

        assertEquals(Date.class, def.getType());
        assertEquals(pit, def.getStart());
        assertEquals(def.getDef(Position.END, pit), def.getEnd());
    }

    /**
     * Tests the usage of a long constructor with null.
     */
    @Test
    public void testLongWithNullValue() {
        final TimelineDefinition def = new TimelineDefinition(null,
                31536000000L, MilliSecond.instance());

        assertEquals(Long.class, def.getType());
        assertEquals(Long.valueOf(0L), def.getStart());
        assertEquals(Long.valueOf(31536000000L), def.getEnd());
    }

    /**
     * Tests the creation of a {@code TimelineDefinition} based on strings.
     */
    @Test
    public void testStringsWithDate() {
        final TimelineDefinition def = new TimelineDefinition("01.01.2000",
                "01.10.2001", MilliSecond.instance());

        assertEquals(Date.class, def.getType());
        assertEquals(getDate("01.01.2000"), def.getStart());
        assertEquals(getDate("01.10.2001"), def.getEnd());
    }

    /**
     * Test the creation using a date-string as start and {@code null} as end.
     */
    @Test
    public void testStringsWithDateAndEndNull() {
        final TimelineDefinition def = new TimelineDefinition("01.01.2000",
                null, MilliSecond.instance());

        assertEquals(Date.class, def.getType());
        assertEquals(getDate("01.01.2000"), def.getStart());
        assertEquals(getDate("01.01.2001"), def.getEnd());
    }

    /**
     * Test the creation using {@code null} as start and a date-string as end.
     */
    @Test
    public void testStringsWithDateAndStartNull() {
        final TimelineDefinition def = new TimelineDefinition(null,
                "01.01.2001", MilliSecond.instance());

        assertEquals(Date.class, def.getType());
        assertEquals(getDate("01.01.2000"), def.getStart());
        assertEquals(getDate("01.01.2001"), def.getEnd());
    }

    /**
     * Tests the creation and manipulation using
     * {@link TimelineDefinition#setDuration(long, Position)}.
     */
    @Test
    public void testSetDurationByDay() {
        final TimelineDefinition def = new TimelineDefinition(null,
                "01.01.2001", MilliSecond.instance());

        def.setDuration(5, Position.END, Day.instance());

        assertEquals(Date.class, def.getType());
        assertEquals(getDate("01.01.2001"), def.getEnd());
        assertEquals(getDate("27.12.2000"), def.getStart());
    }

    /**
     * Tests the creation and manipulation using
     * {@link TimelineDefinition#setDuration(long, Position)}.
     */
    @Test
    public void testSetDurationByYear() {
        final TimelineDefinition def = new TimelineDefinition("01.01.2000",
                null, MilliSecond.instance());

        def.setDuration(2, Position.START, Year.instance());

        assertEquals(Date.class, def.getType());
        assertEquals(getDate("01.01.2000"), def.getStart());
        assertEquals(getDate("01.01.2002"), def.getEnd());
    }

    /**
     * Tests the interpretation of a {@code long} value.
     */
    @Test
    public void testLongByString() {
        final TimelineDefinition def = new TimelineDefinition("0", "100", MilliSecond.instance());

        assertEquals(Long.class, def.getType());
        assertEquals(Long.valueOf(0L), def.getStart());
        assertEquals(Long.valueOf(100L), def.getEnd());
    }

    /**
     * Tests the interpretation of a {@code long} value, whereby the {@code end}
     * is {@code null}.
     */
    @Test
    public void testLongByStringWithNullEnd() {
        final TimelineDefinition def = new TimelineDefinition("10", null,
                MilliSecond.instance());

        assertEquals(Long.class, def.getType());
        assertEquals(Long.valueOf(10L), def.getStart());
        assertEquals(Long.valueOf(10L + TimelineDefinition.OFFSET), def.getEnd());
    }

    /**
     * Tests the interpretation of a {@code long} value, whereby the
     * {@code start} is {@code null}.
     */
    @Test
    public void testLongByStringWithNullStart() {
        final TimelineDefinition def = new TimelineDefinition(null, "100",
                MilliSecond.instance());

        assertEquals(Long.class, def.getType());
        assertEquals(Long.valueOf(99L), def.getStart());
        assertEquals(Long.valueOf(100L), def.getEnd());
    }

    /**
     * Tests defining a date as start and a long value as end.
     */
    @Test
    public void testMixedWithLongAsEnd() {
        final TimelineDefinition def = new TimelineDefinition("29.04.2010",
                "1000", MilliSecond.instance());

        assertEquals(Date.class, def.getType());
        assertEquals(getDate("29.04.2010"), def.getStart());
        assertEquals(getDate("29.04.2010 00:00:01"), def.getEnd());
    }

    /**
     * Tests defining a long value as start and a date as end.
     */
    @Test
    public void testMixedWithLongAsStart() {
        final TimelineDefinition def = new TimelineDefinition("25",
                "29.02.2008", Day.instance());

        assertEquals(Date.class, def.getType());
        assertEquals(getDate("04.02.2008"), def.getStart());
        assertEquals(getDate("29.02.2008"), def.getEnd());
    }

    /**
     * Tests the usage of the default constructor.
     */
    @Test
    public void testDefaultConstructor() {
        final Date now = Dates.truncDate(Dates.now());
        final TimelineDefinition def = new TimelineDefinition();

        assertEquals(Date.class, def.getType());
        assertEquals(Second.instance(), def.getGranularity());
        assertEquals(now.getTime(), def.<Date>getStart().getTime(), 1000);
        assertEquals(DateFormat.YEAR.modify(now, 1).getTime(), def.<Date>getEnd().getTime(), 1000);
    }
}
