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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.Calendar;
import java.util.Locale;

import org.junit.Test;

import fr.landel.utils.assertor.expect.Expect;
import fr.landel.utils.assertor.expect.ExpectException;

/**
 * Test end points class
 *
 * @since 2 août 2016
 * @author Gilles
 *
 */
public class EndPointsTest {
    /**
     * Test method for {@link Operator#toThrow()}.
     */
    @Test
    public void testToThrow() {
        Expect.exception(() -> {
            Assertor.that("text").isEmpty().toThrow();
            fail();
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            Assertor.that("text").isEmpty().toThrow("test");
            fail();
        }, IllegalArgumentException.class, "test");

        Expect.exception(() -> {
            Assertor.that("text").isEmpty().toThrow("test: '%s*'");
            fail();
        }, IllegalArgumentException.class, "test: 'text'");

        Expect.exception(() -> {
            Assertor.that("text").isEmpty().and("").isEmpty().toThrow("%s %2$s test: '%1$s*', '%s*', '%1$s*'.%s*.%2$s*.", "this is", "a");
            fail();
        }, IllegalArgumentException.class, "this is a test: 'text', 'text', 'text'...");

        Expect.exception(() -> {
            Assertor.that("text").isEmpty().toThrow(new IOException());
            fail();
        }, IOException.class);

        Expect.exception(() -> {
            Assertor.that("text").isEmpty("unused message").toThrow("test");
            fail();
        }, IllegalArgumentException.class, "test");
    }

    /**
     * Test method for {@link Operator#toThrow()}.
     */
    @Test
    public void testToThrowReset() {
        AssertCharSequence<String> assertor = Assertor.that("text");
        Operator<AssertCharSequence<String>, String> op = assertor.isEmpty("error");

        assertFalse(op.isOK(false));
        assertEquals("error", op.getErrors(false));
        assertFalse(op.isOK());
        assertEquals(Constants.DEFAULT_ASSERTION, op.getErrors(false));
        assertTrue(op.isOK());

        // clear first operator then second one

        assertor.isEmpty();
        assertFalse(op.isOK(false));

        Operator<AssertCharSequence<String>, String> op2 = assertor.isNotEmpty().and("").isNotEmpty();

        assertFalse(op2.isOK(false));
        assertFalse(op.isOK());
        assertTrue(op.isOK());
        assertFalse(op2.isOK());
        assertTrue(op2.isOK());

        // clear second operator, second has also to be cleared

        assertor.isEmpty();
        assertFalse(op.isOK(false));

        op2 = assertor.isNotEmpty().and("").isNotEmpty();

        assertFalse(op2.isOK(false));
        assertFalse(op2.isOK());
        assertTrue(op2.isOK());
        assertTrue(op.isOK());

        final Operator<AssertCharSequence<String>, String> op3 = assertor.isNotEmpty().and("").isNotEmpty();

        Expect.exception(() -> {
            op3.toThrow(false);
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            op3.toThrow();
        }, IllegalArgumentException.class);

        op3.toThrow();

        final Operator<AssertCharSequence<String>, String> op4 = assertor.isNotEmpty().and("").isNotEmpty();

        Expect.exception(() -> {
            op4.toThrow(false, new IOException());
        }, IOException.class);

        Expect.exception(() -> {
            op4.toThrow();
        }, IllegalArgumentException.class);

        op4.toThrow();
    }

    /**
     * Test method for {@link Operator#toThrow()}.
     */
    @Test(expected = ExpectException.class)
    public void testToThrowException() {
        Expect.exception(() -> {
            Assertor.that("text").isNotEmpty().toThrow();
        }, IllegalArgumentException.class);
    }

    /**
     * Test method for {@link Operator#getErrors()}.
     */
    @Test
    public void testGetErrors() {
        assertEquals("test 265,452", Assertor.that(new BigDecimal("265.45155")).isNull(Locale.FRANCE, "test %1$,.3f*").getErrors());
        assertEquals("test 265.452", Assertor.that(new BigDecimal("265.45155")).isNull(Locale.US, "test %1$,.3f*").getErrors());
        assertEquals("test 2 654 125,452",
                Assertor.that(new BigDecimal("2654125.45155")).isNull(Locale.FRANCE, "test %1$,.3f*").getErrors());
        assertEquals("test 2,654,125.452", Assertor.that(new BigDecimal("2654125.45155")).isNull(Locale.US, "test %1$,.3f*").getErrors());

        Calendar calendar = Calendar.getInstance();
        calendar.set(2016, 6, 25);
        assertEquals("test 25 juillet 2016", Assertor.that(calendar).isNull(Locale.FRANCE, "test %1$te* %1$tB* %1$tY*").getErrors());
        assertEquals("test July 25, 2016", Assertor.that(calendar).isNull(Locale.US, "test %1$tB* %1$te*, %1$tY*").getErrors());
    }
}
