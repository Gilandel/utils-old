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
import static org.junit.Assert.fail;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.Calendar;
import java.util.List;
import java.util.Locale;
import java.util.function.BiFunction;

import org.apache.commons.lang3.tuple.Triple;
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
public class PredicateStepEndPointsTest extends AbstractTest {

    private static final BiFunction<String, List<Triple<Object, EnumType, Boolean>>, IOException> EXCEPTION_BUILDER = (String errors,
            List<Triple<Object, EnumType, Boolean>> parameters) -> new IOException(
                    HelperMessage.getMessage(Constants.DEFAULT_ASSERTION, null, errors, parameters, null));

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
            Assertor.that("text").isEmpty().toThrow((Exception) null, false);
            fail();
        }, IllegalArgumentException.class, "the char sequence 'text' should be null or empty", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that((CharSequence) null).hasLength(-1).toThrow((Exception) null, false);
            fail();
        }, IllegalArgumentException.class, "the length has to be greater than or equal to 0 and the char sequence cannot be null",
                JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that((CharSequence) null).hasLength(-1).toThrow((error, parameters) -> new IOException(String.valueOf(error)));
            fail();
        }, IOException.class, "the length has to be greater than or equal to 0 and the char sequence cannot be null", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that("text").isEmpty().toThrow((CharSequence) null);
            fail();
        }, IllegalArgumentException.class, "the char sequence 'text' should be null or empty", JUNIT_ERROR);

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
            Assertor.that("text").isEmpty().toThrow(new IOException(), false);
            fail();
        }, IOException.class);

        Expect.exception(() -> {
            Assertor.that("text").isEmpty("unused message").toThrow("test");
            fail();
        }, IllegalArgumentException.class, "test");

        Assertor.that("text").isNotEmpty().toThrow(Constants.DEFAULT_EXCEPTION_BUILDER);

        Expect.exception(() -> {
            Assertor.that("text").isEmpty().toThrow(Constants.DEFAULT_EXCEPTION_BUILDER);
        }, IllegalArgumentException.class, "the char sequence 'text' should be null or empty", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that("text").isEmpty().toThrow(EXCEPTION_BUILDER);
            fail();
        }, IOException.class, "the char sequence 'text' should be null or empty", JUNIT_ERROR);
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
