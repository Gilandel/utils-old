/*-
 * #%L
 * utils-assertor
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.assertor;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.Calendar;
import java.util.Date;

import org.junit.Test;

import fr.landel.utils.assertor.expect.Expect;
import fr.landel.utils.commons.DateUtils;

/**
 * Test {@link AssertorDate}
 *
 * @since 29 mai 2016
 * @author Gilles
 *
 */
public class AssertorDateAndCalendarTest extends AbstractTest {

    /**
     * Test method for {@link AssertorDate#AssertorDate()} .
     */
    @Test
    public void testConstructor() {
        assertNotNull(new AssertorDate());
    }

    /**
     * Test method for {@link AssertorDate#isEqual}.
     */
    @Test
    public void testIsEqualOK() {
        final Date date1 = new Date(1464475553640L);
        final Date date2 = new Date(1464475553640L);
        final Calendar calendar1 = DateUtils.getCalendar(date1);
        final Calendar calendar2 = DateUtils.getCalendar(date2);

        assertTrue(Assertor.that(calendar1).isNotNull().isOK());
        assertFalse(Assertor.that(calendar1).not().isAround(calendar2, Calendar.SECOND, 5).isOK());
        assertTrue(Assertor.that(calendar1).isEqual(calendar2).and().isEqual(calendar1).isOK());
        assertTrue(Assertor.that(calendar1).isEqual(calendar2).or().isEqual(calendar1).isOK());
        assertFalse(Assertor.that(calendar1).isEqual(calendar2).xor().isEqual(calendar1).isOK());

        assertFalse(Assertor.that(calendar1).isEqual(calendar2).xor(Assertor.that(true).isTrue()).and().isEqual(calendar1).isOK());

        assertTrue(Assertor.that(date1).isNotNull().isOK());
        assertFalse(Assertor.that(date1).not().isAround(date2, Calendar.SECOND, 5).isOK());
        assertTrue(Assertor.that(date1).isEqual(date2).and().isEqual(date1).isOK());
        assertTrue(Assertor.that(date1).isEqual(date2).or().isEqual(date1).isOK());
        assertFalse(Assertor.that(date1).isEqual(date2).xor().isEqual(date1).isOK());

        assertFalse(Assertor.that(date1).isEqual(date2).xor(Assertor.that(true).isTrue()).and().isEqual(date1).isOK());

        try {
            Assertor.that((Date) null).isEqual((Date) null).toThrow();
            Assertor.that((Calendar) null).isEqual((Calendar) null).toThrow();

            Assertor.that(date1).isEqual(date2).toThrow();

            Assertor.that(calendar1).isEqual(calendar2).toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }

        Expect.exception(() -> {
            Assertor.that(date1).isEqual(calendar2).toThrow();
            fail();
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            Assertor.that(calendar1).isEqual(date2).toThrow();
            fail();
        }, IllegalArgumentException.class);
    }

    /**
     * Test method for {@link AssertorDate#isEqual}.
     */
    @Test
    public void testIsEqualKO() {
        final Date date1 = new Date(1464475553640L);
        final Date date2 = new Date(1464475553641L);

        Assertor.that(date1).isAround(date2, Calendar.SECOND, 5).toThrow();
        Assertor.that(date1).isAround(date1, Calendar.SECOND, 5).toThrow();

        try {
            Assertor.that(date1).isEqual(date2).toThrow();
            Assertor.that((Date) null).isEqual(date2).and().isEqual(date2).toThrow();
            fail();
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        Expect.exception(() -> {
            Assertor.that(date1).isEqual((Date) null).toThrow();
            fail();
        }, IllegalArgumentException.class);

        try {
            Assertor.that((Date) null).isEqual(date2).toThrow();
            fail();
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link AssertCalendar#isAround}.
     */
    @Test
    public void testIsAroundOK() {
        try {
            final Calendar calendar1 = Calendar.getInstance();
            final Calendar calendar2 = Calendar.getInstance();

            calendar1.set(2016, 05, 29, 5, 5, 6);
            calendar2.set(2016, 05, 29, 5, 5, 5);

            Assertor.that(calendar1).isAround(calendar2, Calendar.SECOND, 5).toThrow();

            calendar1.set(2016, 05, 29, 5, 5, 1);

            Assertor.that(calendar1).isAround(calendar2, Calendar.SECOND, 5).toThrow();

            assertFalse(Assertor.that(calendar1).isAround(calendar2, -1, 2).isOK());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertCalendar#isAround}.
     */
    @Test
    public void testIsAroundKO() {
        Calendar c1 = Calendar.getInstance();
        Calendar c2 = Calendar.getInstance();

        c1.set(2016, 05, 29, 5, 5, 9);
        c2.set(2016, 05, 29, 5, 5, 5);

        try {
            // Check is date1 is not around the date2 by max 5s (after)
            Assertor.that(c1).isAround(c2, Calendar.SECOND, 2).toThrow();
            fail();
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check date1 = null
            Assertor.that((Calendar) null).isAround(c2, Calendar.SECOND, 2).toThrow();
            fail();
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check date1 = null
            Assertor.that((Calendar) null).isAround(c2, -1, -1).toThrow();
            fail();
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check date2 = null
            Assertor.that(c1).isAround((Calendar) null, Calendar.SECOND, 2).toThrow();
            fail();
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check calendar amount = zero
            Assertor.that(c1).isAround(c2, Calendar.SECOND, 0).toThrow();
            fail();
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check unsupported calendar field
            Assertor.that(c1).isAround(c2, 20, 0).toThrow();
            fail();
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        c2.set(2016, 05, 29, 5, 5, 13);

        try {
            // Check is date1 is not around the date2 by max 5s (before)
            Assertor.that(c1).isAround(c2, Calendar.SECOND, 2).toThrow();
            fail();
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link AssertCalendar#isNotAround}.
     */
    @Test
    public void testIsNotAroundOK() {
        final Calendar c1 = Calendar.getInstance();
        final Calendar c2 = Calendar.getInstance();

        c1.set(2016, 05, 29, 5, 5, 11);
        c2.set(2016, 05, 29, 5, 5, 5);

        assertTrue(Assertor.that(c1).not().isAround(c2, Calendar.SECOND, 5).isOK());
        assertTrue(Assertor.that(c1).isNotAround(c2, Calendar.SECOND, 5).isOK());

        assertTrue(Assertor.that(c1.getTime()).not().isAround(c2.getTime(), Calendar.SECOND, 5).isOK());
        assertTrue(Assertor.that(c1.getTime()).isNotAround(c2.getTime(), Calendar.SECOND, 5).isOK());

        c1.set(2016, 05, 29, 5, 5, 1);

        assertTrue(Assertor.that(c1).isNotAround(c2, Calendar.SECOND, 3).isOK());
        assertFalse(Assertor.that(c1).isNotAround(c2, Calendar.SECOND, 0).isOK());
        assertTrue(Assertor.that(c1).isNotAround(c2, -1, 0).isOK());

        Expect.exception(() -> {
            Assertor.that((Calendar) null).isNotAround(c2, Calendar.SECOND, 5).toThrow();
            fail();
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            Assertor.that(c1).isNotAround((Calendar) null, Calendar.SECOND, 5).toThrow();
            fail();
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            Assertor.that(c1).isNotAround((Calendar) null, Calendar.SECOND, 5).toThrow();
            fail();
        }, IllegalArgumentException.class,
                "neither dates cannot be null, calendar field has to be a supported value and calendar amount different to 0", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that(c1.getTime()).isNotAround((Date) null, Calendar.SECOND, 5).toThrow();
            fail();
        }, IllegalArgumentException.class,
                "neither dates cannot be null, calendar field has to be a supported value and calendar amount different to 0", JUNIT_ERROR);
    }

    /**
     * Test method for {@link AssertCalendar#isNotAround}.
     */
    @Test
    public void testIsNotAroundKO() {
        final Calendar c1 = Calendar.getInstance();
        final Calendar c2 = Calendar.getInstance();

        c1.set(2016, 05, 29, 5, 5, 9);
        c2.set(2016, 05, 29, 5, 5, 5);

        try {
            // Check is date1 is not around the date2 by max 5s (after)
            Assertor.that(c1).isNotAround(c2, Calendar.SECOND, 5).toThrow();
            fail();
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check calendar amount = zero
            Assertor.that(c1).isNotAround(c2, Calendar.SECOND, 0).toThrow();
            fail();
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check unsupported calendar field
            Assertor.that(c1).isNotAround(c2, 20, 0).toThrow();
            fail();
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        c2.set(2016, 05, 29, 5, 5, 11);

        try {
            // Check is date1 is not around the date2 by max 5s (before)
            Assertor.that(c1).isNotAround(c2, Calendar.SECOND, 5).toThrow();
            fail();
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link AssertorDate#isNotEqual}.
     */
    @Test
    public void testIsNotEqualOK() {
        try {
            final Date date1 = new Date(1464475553640L);
            final Date date2 = new Date(1464475553641L);

            Assertor.that(DateUtils.getCalendar(date1)).isNotEqual(DateUtils.getCalendar(date2)).toThrow();
            Assertor.that(date1).isNotEqual(date2).toThrow();
            Assertor.that((Date) null).isNotEqual(date2).toThrow();
            Assertor.that(date1).isNotEqual((Date) null).toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertorDate#isNotEqual}.
     */
    @Test
    public void testIsNotEqualKO() {
        Date date1 = new Date(1464475553640L);
        Date date2 = new Date(1464475553640L);

        try {
            Assertor.that(date1).isNotEqual(date2).toThrow();
            fail();
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that((Date) null).isNotEqual((Date) null).toThrow();
            fail();
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link AssertorDate#isAfter}.
     */
    @Test
    public void testIsAfterOK() {
        try {
            Date date1 = new Date(1464475553641L);
            Date date2 = new Date(1464475553640L);

            Assertor.that(date1).isAfter(date2).toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertCalendar#isAfter}.
     */
    @Test
    public void testIsAfterKO() {
        final Date date1 = new Date(1464475553640L);
        Date date2 = new Date(1464475553641L);
        Calendar calendar1 = DateUtils.getCalendar(date1);
        Calendar calendar2 = DateUtils.getCalendar(date2);

        assertFalse(Assertor.that(date1).isAfter(date2).isOK());
        assertFalse(Assertor.that(calendar1).isAfter(calendar2).isOK());

        date2 = new Date(1464475553640L);

        try {
            Assertor.that(date1).isAfter(date2).toThrow();
            fail();
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that((Date) null).isAfter(date2).toThrow();
            fail();
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that((Date) null).isAfter((Date) null).toThrow();
            fail();
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that(date1).isAfter((Date) null).toThrow();
            fail();
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that(date1).isAfter(date2).toThrow(new IOException(), true);
            fail();
        } catch (IOException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link AssertorDate#isAfterOrEqual}.
     */
    @Test
    public void testIsAfterOrEqualOK() {
        try {
            Date date1 = new Date(1464475553641L);
            final Date date2 = new Date(1464475553640L);

            Assertor.that(date1).isAfterOrEquals(date2).toThrow();
            Assertor.that(DateUtils.getCalendar(date1)).isAfterOrEquals(DateUtils.getCalendar(date2)).toThrow();

            date1 = new Date(1464475553640L);

            Assertor.that(date1).isAfterOrEquals(date2).toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertorDate#isAfterOrEqual}.
     */
    @Test
    public void testIsAfterOrEqualKO() {
        Date date1 = new Date(1464475553640L);
        Date date2 = new Date(1464475553641L);

        assertFalse(Assertor.that(date1).isAfterOrEquals(date2).isOK());
        assertFalse(Assertor.that(DateUtils.getCalendar(date1)).isAfterOrEquals(DateUtils.getCalendar(date2)).isOK());

        Expect.exception(() -> {
            Assertor.that((Date) null).isAfterOrEquals(date2).toThrow();
            fail();
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            Assertor.that(date1).isAfterOrEquals((Date) null).toThrow();
            fail();
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            Assertor.that((Date) null).isAfterOrEquals((Date) null).toThrow();
            fail();
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            Assertor.that(date1).isAfterOrEquals(date2).toThrow(new IOException(), true);
            fail();
        }, IOException.class);
    }

    /**
     * Test method for {@link AssertorDate#isBefore}.
     */
    @Test
    public void testIsBefore() {
        final Date date1 = new Date(1464475553641L);
        final Date date2 = new Date(1464475553640L);

        assertTrue(Assertor.that(date2).isBefore(date1).isOK());
        assertTrue(Assertor.that(DateUtils.getCalendar(date2)).isBefore(DateUtils.getCalendar(date1)).isOK());

        Expect.exception(() -> {
            Assertor.that(date1).isBefore(date2).toThrow();
            fail();
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            Assertor.that(date1).isBefore(date1).toThrow();
            fail();
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            Assertor.that((Date) null).isBefore(date1).toThrow();
            fail();
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            Assertor.that(date1).isBefore((Date) null).toThrow();
            fail();
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            Assertor.that((Date) null).isBefore((Date) null).toThrow();
            fail();
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            Assertor.that(date1).isBefore(date1).toThrow(new IOException(), true);
            fail();
        }, IOException.class);
    }

    /**
     * Test method for {@link AssertorDate#isBeforeOrEqual}.
     */
    @Test
    public void testIsBeforeOrEqual() {
        Date date1 = new Date(1464475553640L);
        final Date date2 = new Date(1464475553641L);

        assertTrue(Assertor.that(date1).isBeforeOrEquals(date2).isOK());
        assertTrue(Assertor.that(DateUtils.getCalendar(date1)).isBeforeOrEquals(DateUtils.getCalendar(date2)).isOK());

        date1 = new Date(1464475553641L);

        assertTrue(Assertor.that(date1).isBeforeOrEquals(date2).isOK());

        final Date date3 = new Date(1464475553641L);
        final Date date4 = new Date(1464475553640L);

        Expect.exception(() -> {
            Assertor.that(date3).isBeforeOrEquals(date4).toThrow();
            fail();
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            Assertor.that((Date) null).isBeforeOrEquals(date4).toThrow();
            fail();
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            Assertor.that(date3).isBeforeOrEquals((Date) null).toThrow();
            fail();
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            Assertor.that((Date) null).isBeforeOrEquals((Date) null).toThrow();
            fail();
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            Assertor.that(date3).isBeforeOrEquals(date4).toThrow(new IOException(), true);
            fail();
        }, IOException.class);
    }
}