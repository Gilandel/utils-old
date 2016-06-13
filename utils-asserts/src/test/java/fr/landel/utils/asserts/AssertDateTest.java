/*-
 * #%L
 * utils-asserts
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.asserts;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.Calendar;
import java.util.Date;

import org.junit.Test;

import fr.landel.utils.commons.DateUtils;

/**
 * Test date assert
 *
 * @since 29 mai 2016
 * @author Gilles
 *
 */
public class AssertDateTest {

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
            AssertUtils.check((Date) null).isEqual((Date) null);
            AssertUtils.check((Calendar) null).isEqual((Calendar) null);

            AssertUtils.check(date1).isEqual(date2).isEqual(date2, "error not equal").isEqual(date2, new IllegalArgumentException());

            AssertUtils.check(calendar1).isEqual(calendar2).isEqual(calendar2, "error not equal").isEqual(calendar2,
                    new IllegalArgumentException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }

        try {
            AssertUtils.check(date1).isEqual(calendar2).isEqual(calendar2, "error not equal").isEqual(calendar2,
                    new IllegalArgumentException());
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.check(calendar1).isEqual(date2).isEqual(date2, "error not equal").isEqual(date2, new IllegalArgumentException());
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

        try {
            AssertUtils.check(date1).isEqual(date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.check(date1).isEqual((Date) null);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.check((Date) null).isEqual(date2);
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

            AssertUtils.check(calendar1).isAround(calendar2, Calendar.SECOND, 5).isAround(calendar2, Calendar.SECOND, 5, "error")
                    .isAround(calendar2, Calendar.SECOND, 5, new IllegalArgumentException());

            calendar1.set(2016, 05, 29, 5, 5, 1);

            AssertUtils.check(calendar1).isAround(calendar2, Calendar.SECOND, 5);
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
            AssertUtils.check(c1).isAround(c2, Calendar.SECOND, 2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check date1 = null
            AssertUtils.check((Calendar) null).isAround(c2, Calendar.SECOND, 2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check date2 = null
            AssertUtils.check(c1).isAround((Calendar) null, Calendar.SECOND, 2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check calendar amount = zero
            AssertUtils.check(c1).isAround(c2, Calendar.SECOND, 0);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check unsupported calendar field
            AssertUtils.check(c1).isAround(c2, 20, 0);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        c2.set(2016, 05, 29, 5, 5, 13);

        try {
            // Check is date1 is not around the date2 by max 5s (before)
            AssertUtils.check(c1).isAround(c2, Calendar.SECOND, 2);
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

            AssertUtils.check(c1).isNotAround(c2, Calendar.SECOND, 5);
            AssertUtils.check(c1).isNotAround(c2, Calendar.SECOND, 5, "error");
            AssertUtils.check(c1).isNotAround(c2, Calendar.SECOND, 5, new IllegalArgumentException());

            AssertUtils.check((Calendar) null).isNotAround(c2, Calendar.SECOND, 5);
            AssertUtils.check(c1).isNotAround((Calendar) null, Calendar.SECOND, 5);

            c1.set(2016, 05, 29, 5, 5, 1);

            AssertUtils.check(c1).isNotAround(c2, Calendar.SECOND, 3);
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
            AssertUtils.check(c1).isNotAround(c2, Calendar.SECOND, 5);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check calendar amount = zero
            AssertUtils.check(c1).isNotAround(c2, Calendar.SECOND, 0);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check unsupported calendar field
            AssertUtils.check(c1).isNotAround(c2, 20, 0);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        c2.set(2016, 05, 29, 5, 5, 11);

        try {
            // Check is date1 is not around the date2 by max 5s (before)
            AssertUtils.check(c1).isNotAround(c2, Calendar.SECOND, 5);
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

            AssertUtils.check(date1).isNotEqual(date2);
            AssertUtils.check((Date) null).isNotEqual(date2);
            AssertUtils.check(date1).isNotEqual((Date) null).isNotEqual(date2, "error equal").isNotEqual(date2,
                    new IllegalArgumentException());
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
            AssertUtils.check(date1).isNotEqual(date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.check((Date) null).isNotEqual((Date) null);
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

            AssertUtils.check(date1).isAfter(date2).isAfter(date2, "error not equal").isAfter(date2, new IllegalArgumentException());
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

        try {
            AssertUtils.check(date1).isAfter(date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        date2 = new Date(1464475553640L);

        try {
            AssertUtils.check(date1).isAfter(date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.check((Date) null).isAfter(date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.check((Date) null).isAfter((Date) null);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.check(date1).isAfter((Date) null);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.check(date1).isAfter(date2, new IOException());
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

            AssertUtils.check(date1).isAfterOrEqual(date2).isAfterOrEqual(date2, "error not equal").isAfterOrEqual(date2,
                    new IllegalArgumentException());

            date1 = new Date(1464475553640L);

            AssertUtils.check(date1).isAfterOrEqual(date2).isAfterOrEqual(date2, "error not equal").isAfterOrEqual(date2,
                    new IllegalArgumentException());
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

        try {
            AssertUtils.check(date1).isAfterOrEqual(date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.check((Date) null).isAfterOrEqual(date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.check(date1).isAfterOrEqual((Date) null);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.check((Date) null).isAfterOrEqual((Date) null);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.check(date1).isAfterOrEqual(date2, new IOException());
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

            AssertUtils.check(date1).isBefore(date2).isBefore(date2, "error not equal").isBefore(date2, new IllegalArgumentException());
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
            AssertUtils.check(date1).isBefore(date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        date2 = new Date(1464475553641L);

        try {
            AssertUtils.check(date1).isBefore(date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.check((Date) null).isBefore(date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.check(date1).isBefore((Date) null);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.check((Date) null).isBefore((Date) null);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.check(date1).isBefore(date2, new IOException());
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

            AssertUtils.check(date1).isBeforeOrEqual(date2);
            AssertUtils.check(date1).isBeforeOrEqual(date2, "error not equal");
            AssertUtils.check(date1).isBeforeOrEqual(date2, new IllegalArgumentException());

            date1 = new Date(1464475553641L);

            AssertUtils.check(date1).isBeforeOrEqual(date2);
            AssertUtils.check(date1).isBeforeOrEqual(date2, "error not equal");
            AssertUtils.check(date1).isBeforeOrEqual(date2, new IllegalArgumentException());
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
            AssertUtils.check(date1).isBeforeOrEqual(date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.check((Date) null).isBeforeOrEqual(date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.check(date1).isBeforeOrEqual((Date) null);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.check((Date) null).isBeforeOrEqual((Date) null);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.check(date1).isBeforeOrEqual(date2, new IOException());
            fail("Has to raise an exception");
        } catch (IOException e) {
            assertNotNull(e);
        }
    }
}