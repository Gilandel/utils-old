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
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.Calendar;
import java.util.Date;

import org.junit.Test;

import fr.landel.utils.assertor.Assertor;
import fr.landel.utils.assertor.Expect;
import fr.landel.utils.commons.DateUtils;

/**
 * Test date assert
 *
 * @since 29 mai 2016
 * @author Gilles
 *
 */
public class AssertDateAndCalendarTest {

    /**
     * Test method for {@link Expect#isEqual}.
     */
    @Test
    public void testIsEqualOK() {
        final Date date1 = new Date(1464475553640L);
        final Date date2 = new Date(1464475553640L);
        final Calendar calendar1 = DateUtils.getCalendar(date1);
        final Calendar calendar2 = DateUtils.getCalendar(date2);

        try {
            Assertor.that((Date) null).isEqual((Date) null).toThrow();
            Assertor.that((Calendar) null).isEqual((Calendar) null).toThrow();

            Assertor.that(date1).isEqual(date2).toThrow();

            Assertor.that(calendar1).isEqual(calendar2).toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }

        try {
            Assertor.that(date1).isEqual(calendar2).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that(calendar1).isEqual(date2).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Expect#isEqual}.
     */
    @Test
    public void testIsEqualKO() {
        final Date date1 = new Date(1464475553640L);
        final Date date2 = new Date(1464475553641L);

        Assertor.that(date1).isAround(date2, Calendar.SECOND, 5).toThrow();

        try {
            Assertor.that(date1).isEqual(date2).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that(date1).isEqual((Date) null).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that((Date) null).isEqual(date2).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Expect#isAround}.
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
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isAround}.
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
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check date1 = null
            Assertor.that((Calendar) null).isAround(c2, Calendar.SECOND, 2).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check date2 = null
            Assertor.that(c1).isAround((Calendar) null, Calendar.SECOND, 2).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check calendar amount = zero
            Assertor.that(c1).isAround(c2, Calendar.SECOND, 0).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check unsupported calendar field
            Assertor.that(c1).isAround(c2, 20, 0).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        c2.set(2016, 05, 29, 5, 5, 13);

        try {
            // Check is date1 is not around the date2 by max 5s (before)
            Assertor.that(c1).isAround(c2, Calendar.SECOND, 2).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Expect#isNotAround}.
     */
    @Test
    public void testIsNotAroundOK() {
        try {
            Calendar c1 = Calendar.getInstance();
            Calendar c2 = Calendar.getInstance();

            c1.set(2016, 05, 29, 5, 5, 11);
            c2.set(2016, 05, 29, 5, 5, 5);

            Assertor.that(c1).isNotAround(c2, Calendar.SECOND, 5).toThrow();

            Assertor.that((Calendar) null).isNotAround(c2, Calendar.SECOND, 5).toThrow();
            Assertor.that(c1).isNotAround((Calendar) null, Calendar.SECOND, 5).toThrow();

            c1.set(2016, 05, 29, 5, 5, 1);

            Assertor.that(c1).isNotAround(c2, Calendar.SECOND, 3).toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isNotAround}.
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
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check calendar amount = zero
            Assertor.that(c1).isNotAround(c2, Calendar.SECOND, 0).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check unsupported calendar field
            Assertor.that(c1).isNotAround(c2, 20, 0).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        c2.set(2016, 05, 29, 5, 5, 11);

        try {
            // Check is date1 is not around the date2 by max 5s (before)
            Assertor.that(c1).isNotAround(c2, Calendar.SECOND, 5).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Expect#isNotEqual}.
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
     * Test method for {@link Expect#isNotEqual}.
     */
    @Test
    public void testIsNotEqualKO() {
        Date date1 = new Date(1464475553640L);
        Date date2 = new Date(1464475553640L);

        try {
            Assertor.that(date1).isNotEqual(date2).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that((Date) null).isNotEqual((Date) null).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Expect#isAfter}.
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
     * Test method for {@link Expect#isAfter}.
     */
    @Test
    public void testIsAfterKO() {
        final Date date1 = new Date(1464475553640L);
        Date date2 = new Date(1464475553641L);
        Calendar calendar1 = DateUtils.getCalendar(date1);
        Calendar calendar2 = DateUtils.getCalendar(date2);

        assertFalse(Assertor.that(date1).isAfter(date2).getResult());
        assertFalse(Assertor.that(calendar1).isAfter(calendar2).getResult());

        date2 = new Date(1464475553640L);

        try {
            Assertor.that(date1).isAfter(date2).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that((Date) null).isAfter(date2).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that((Date) null).isAfter((Date) null).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that(date1).isAfter((Date) null).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that(date1).isAfter(date2).toThrow(new IOException());
            fail("Has to raise an exception");
        } catch (IOException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Expect#isAfterOrEqual}.
     */
    @Test
    public void testIsAfterOrEqualOK() {
        try {
            Date date1 = new Date(1464475553641L);
            final Date date2 = new Date(1464475553640L);

            Assertor.that(date1).isAfterOrEqual(date2).toThrow();
            Assertor.that(DateUtils.getCalendar(date1)).isAfterOrEqual(DateUtils.getCalendar(date2)).toThrow();

            date1 = new Date(1464475553640L);

            Assertor.that(date1).isAfterOrEqual(date2).toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isAfterOrEqual}.
     */
    @Test
    public void testIsAfterOrEqualKO() {
        Date date1 = new Date(1464475553640L);
        Date date2 = new Date(1464475553641L);

        assertFalse(Assertor.that(date1).isAfterOrEqual(date2).getResult());
        assertFalse(Assertor.that(DateUtils.getCalendar(date1)).isAfterOrEqual(DateUtils.getCalendar(date2)).getResult());

        try {
            Assertor.that((Date) null).isAfterOrEqual(date2).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that(date1).isAfterOrEqual((Date) null).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that((Date) null).isAfterOrEqual((Date) null).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that(date1).isAfterOrEqual(date2).toThrow(new IOException());
            fail("Has to raise an exception");
        } catch (IOException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Expect#isBefore}.
     */
    @Test
    public void testIsBeforeOK() {
        try {
            final Date date1 = new Date(1464475553640L);
            final Date date2 = new Date(1464475553641L);

            Assertor.that(date1).isBefore(date2).toThrow();
            Assertor.that(DateUtils.getCalendar(date1)).isBefore(DateUtils.getCalendar(date2)).toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isBefore}.
     */
    @Test
    public void testIsBeforeKO() {
        final Date date1 = new Date(1464475553641L);
        Date date2 = new Date(1464475553640L);

        try {
            Assertor.that(date1).isBefore(date2).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        date2 = new Date(1464475553641L);

        try {
            Assertor.that(date1).isBefore(date2).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that((Date) null).isBefore(date2).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that(date1).isBefore((Date) null).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that((Date) null).isBefore((Date) null).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that(date1).isBefore(date2).toThrow(new IOException());
            fail("Has to raise an exception");
        } catch (IOException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Expect#isBeforeOrEqual}.
     */
    @Test
    public void testIsBeforeOrEqualOK() {
        try {
            Date date1 = new Date(1464475553640L);
            final Date date2 = new Date(1464475553641L);

            Assertor.that(date1).isBeforeOrEqual(date2).toThrow();
            Assertor.that(DateUtils.getCalendar(date1)).isBeforeOrEqual(DateUtils.getCalendar(date2)).toThrow();

            date1 = new Date(1464475553641L);

            Assertor.that(date1).isBeforeOrEqual(date2).toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isBeforeOrEqual}.
     */
    @Test
    public void testIsBeforeOrEqualKO() {
        final Date date1 = new Date(1464475553641L);
        final Date date2 = new Date(1464475553640L);

        try {
            Assertor.that(date1).isBeforeOrEqual(date2).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that((Date) null).isBeforeOrEqual(date2).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that(date1).isBeforeOrEqual((Date) null).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that((Date) null).isBeforeOrEqual((Date) null).toThrow();
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            Assertor.that(date1).isBeforeOrEqual(date2).toThrow(new IOException());
            fail("Has to raise an exception");
        } catch (IOException e) {
            assertNotNull(e);
        }
    }
}
