package fr.landel.utils.commons.asserts;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.Calendar;
import java.util.Date;

import org.junit.Test;

/**
 * Test date assert
 *
 * @since 29 mai 2016
 * @author Gilles
 *
 */
public class AbstractDateAssertTest {

    /**
     * Test method for {@link AbstractDateAssert#isEqual}.
     */
    @Test
    public void testIsEqualOK() {
        try {
            Date date1 = new Date(1464475553640L);
            Date date2 = new Date(1464475553640L);

            AssertUtils.isEqual(date1, date2);
            AssertUtils.isEqual((Date) null, null);
            AssertUtils.isEqual(date1, date2, "error not equal");
            AssertUtils.isEqual(date1, date2, new IllegalArgumentException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AbstractDateAssert#isEqual}.
     */
    @Test
    public void testIsEqualKO() {
        Date date1 = new Date(1464475553640L);
        Date date2 = new Date(1464475553641L);

        try {
            AssertUtils.isEqual(date1, date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.isEqual(date1, null);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.isEqual(null, date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link AbstractDateAssert#isAround}.
     */
    @Test
    public void testIsAroundOK() {
        try {
            Calendar c1 = Calendar.getInstance();
            Calendar c2 = Calendar.getInstance();

            c1.set(2016, 05, 29, 5, 5, 6);
            c2.set(2016, 05, 29, 5, 5, 5);

            AssertUtils.isAround(c1.getTime(), c2.getTime(), Calendar.SECOND, 5);
            AssertUtils.isAround(c1.getTime(), c2.getTime(), Calendar.SECOND, 5, "error");
            AssertUtils.isAround(c1.getTime(), c2.getTime(), Calendar.SECOND, 5, new IllegalArgumentException());

            c1.set(2016, 05, 29, 5, 5, 1);

            AssertUtils.isAround(c1.getTime(), c2.getTime(), Calendar.SECOND, 5);
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AbstractDateAssert#isAround}.
     */
    @Test
    public void testIsAroundKO() {
        Calendar c1 = Calendar.getInstance();
        Calendar c2 = Calendar.getInstance();

        c1.set(2016, 05, 29, 5, 5, 9);
        c2.set(2016, 05, 29, 5, 5, 5);

        try {
            // Check is date1 is not around the date2 by max 5s (after)
            AssertUtils.isAround(c1.getTime(), c2.getTime(), Calendar.SECOND, 2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check date1 = null
            AssertUtils.isAround(null, c2.getTime(), Calendar.SECOND, 2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check date2 = null
            AssertUtils.isAround(c1.getTime(), null, Calendar.SECOND, 2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check calendar amount = zero
            AssertUtils.isAround(c1.getTime(), c2.getTime(), Calendar.SECOND, 0);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check unsupported calendar field
            AssertUtils.isAround(c1.getTime(), c2.getTime(), 20, 0);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        c2.set(2016, 05, 29, 5, 5, 13);

        try {
            // Check is date1 is not around the date2 by max 5s (before)
            AssertUtils.isAround(c1.getTime(), c2.getTime(), Calendar.SECOND, 2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link AbstractDateAssert#isNotAround}.
     */
    @Test
    public void testIsNotAroundOK() {
        try {
            Calendar c1 = Calendar.getInstance();
            Calendar c2 = Calendar.getInstance();

            c1.set(2016, 05, 29, 5, 5, 11);
            c2.set(2016, 05, 29, 5, 5, 5);

            AssertUtils.isNotAround(c1.getTime(), c2.getTime(), Calendar.SECOND, 5);
            AssertUtils.isNotAround(c1.getTime(), c2.getTime(), Calendar.SECOND, 5, "error");
            AssertUtils.isNotAround(c1.getTime(), c2.getTime(), Calendar.SECOND, 5, new IllegalArgumentException());

            AssertUtils.isNotAround(null, c2.getTime(), Calendar.SECOND, 5);
            AssertUtils.isNotAround(c1.getTime(), null, Calendar.SECOND, 5);

            c1.set(2016, 05, 29, 5, 5, 1);

            AssertUtils.isNotAround(c1.getTime(), c2.getTime(), Calendar.SECOND, 3);
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AbstractDateAssert#isNotAround}.
     */
    @Test
    public void testIsNotAroundKO() {
        Calendar c1 = Calendar.getInstance();
        Calendar c2 = Calendar.getInstance();

        c1.set(2016, 05, 29, 5, 5, 9);
        c2.set(2016, 05, 29, 5, 5, 5);

        try {
            // Check is date1 is not around the date2 by max 5s (after)
            AssertUtils.isNotAround(c1.getTime(), c2.getTime(), Calendar.SECOND, 5);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check calendar amount = zero
            AssertUtils.isNotAround(c1.getTime(), c2.getTime(), Calendar.SECOND, 0);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            // Check unsupported calendar field
            AssertUtils.isNotAround(c1.getTime(), c2.getTime(), 20, 0);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        c2.set(2016, 05, 29, 5, 5, 11);

        try {
            // Check is date1 is not around the date2 by max 5s (before)
            AssertUtils.isNotAround(c1.getTime(), c2.getTime(), Calendar.SECOND, 5);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link AbstractDateAssert#isNotEqual}.
     */
    @Test
    public void testIsNotEqualOK() {
        try {
            Date date1 = new Date(1464475553640L);
            Date date2 = new Date(1464475553641L);

            AssertUtils.isNotEqual(date1, date2);
            AssertUtils.isNotEqual(null, date2);
            AssertUtils.isNotEqual(date1, null);
            AssertUtils.isNotEqual(date1, date2, "error equal");
            AssertUtils.isNotEqual(date1, date2, new IllegalArgumentException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AbstractDateAssert#isNotEqual}.
     */
    @Test
    public void testIsNotEqualKO() {
        Date date1 = new Date(1464475553640L);
        Date date2 = new Date(1464475553640L);

        try {
            AssertUtils.isNotEqual(date1, date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.isNotEqual((Date) null, null);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link AbstractDateAssert#isAfter}.
     */
    @Test
    public void testIsAfterOK() {
        try {
            Date date1 = new Date(1464475553641L);
            Date date2 = new Date(1464475553640L);

            AssertUtils.isAfter(date1, date2);
            AssertUtils.isAfter(date1, date2, "error not equal");
            AssertUtils.isAfter(date1, date2, new IllegalArgumentException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AbstractDateAssert#isAfter}.
     */
    @Test
    public void testIsAfterKO() {
        Date date1 = new Date(1464475553640L);
        Date date2 = new Date(1464475553641L);

        try {
            AssertUtils.isAfter(date1, date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        date2 = new Date(1464475553640L);

        try {
            AssertUtils.isAfter(date1, date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.isAfter(null, date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.isAfter(date1, null);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.isAfter(date1, date2, new IOException());
            fail("Has to raise an exception");
        } catch (IOException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link AbstractDateAssert#isAfterOrEqual}.
     */
    @Test
    public void testIsAfterOrEqualOK() {
        try {
            Date date1 = new Date(1464475553641L);
            Date date2 = new Date(1464475553640L);

            AssertUtils.isAfterOrEqual(date1, date2);
            AssertUtils.isAfterOrEqual(date1, date2, "error not equal");
            AssertUtils.isAfterOrEqual(date1, date2, new IllegalArgumentException());

            date1 = new Date(1464475553640L);

            AssertUtils.isAfterOrEqual(date1, date2);
            AssertUtils.isAfterOrEqual(date1, date2, "error not equal");
            AssertUtils.isAfterOrEqual(date1, date2, new IllegalArgumentException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AbstractDateAssert#isAfterOrEqual}.
     */
    @Test
    public void testIsAfterOrEqualKO() {
        Date date1 = new Date(1464475553640L);
        Date date2 = new Date(1464475553641L);

        try {
            AssertUtils.isAfterOrEqual(date1, date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.isAfterOrEqual(null, date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.isAfterOrEqual(date1, null);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.isAfterOrEqual(date1, date2, new IOException());
            fail("Has to raise an exception");
        } catch (IOException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link AbstractDateAssert#isBefore}.
     */
    @Test
    public void testIsBeforeOK() {
        try {
            Date date1 = new Date(1464475553640L);
            Date date2 = new Date(1464475553641L);

            AssertUtils.isBefore(date1, date2);
            AssertUtils.isBefore(date1, date2, "error not equal");
            AssertUtils.isBefore(date1, date2, new IllegalArgumentException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AbstractDateAssert#isBefore}.
     */
    @Test
    public void testIsBeforeKO() {
        Date date1 = new Date(1464475553641L);
        Date date2 = new Date(1464475553640L);

        try {
            AssertUtils.isBefore(date1, date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        date2 = new Date(1464475553641L);

        try {
            AssertUtils.isBefore(date1, date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.isBefore(null, date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.isBefore(date1, null);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.isBefore(date1, date2, new IOException());
            fail("Has to raise an exception");
        } catch (IOException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link AbstractDateAssert#isBeforeOrEqual}.
     */
    @Test
    public void testIsBeforeOrEqualOK() {
        try {
            Date date1 = new Date(1464475553640L);
            Date date2 = new Date(1464475553641L);

            AssertUtils.isBeforeOrEqual(date1, date2);
            AssertUtils.isBeforeOrEqual(date1, date2, "error not equal");
            AssertUtils.isBeforeOrEqual(date1, date2, new IllegalArgumentException());

            date1 = new Date(1464475553641L);

            AssertUtils.isBeforeOrEqual(date1, date2);
            AssertUtils.isBeforeOrEqual(date1, date2, "error not equal");
            AssertUtils.isBeforeOrEqual(date1, date2, new IllegalArgumentException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AbstractDateAssert#isBeforeOrEqual}.
     */
    @Test
    public void testIsBeforeOrEqualKO() {
        Date date1 = new Date(1464475553641L);
        Date date2 = new Date(1464475553640L);

        try {
            AssertUtils.isBeforeOrEqual(date1, date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.isBeforeOrEqual(null, date2);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.isBeforeOrEqual(date1, null);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }

        try {
            AssertUtils.isBeforeOrEqual(date1, date2, new IOException());
            fail("Has to raise an exception");
        } catch (IOException e) {
            assertNotNull(e);
        }
    }
}
