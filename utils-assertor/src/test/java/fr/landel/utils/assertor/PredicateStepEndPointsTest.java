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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.Calendar;
import java.util.List;
import java.util.Locale;
import java.util.NoSuchElementException;
import java.util.function.BiFunction;
import java.util.function.Supplier;

import org.junit.Test;

import fr.landel.utils.assertor.expect.Expect;
import fr.landel.utils.assertor.expect.ExpectException;

/**
 * Test end points class
 *
 * @since Aug 2, 2016
 * @author Gilles
 *
 */
public class PredicateStepEndPointsTest extends AbstractTest {

    /**
     * Default exception builder
     */
    protected static final BiFunction<String, List<Parameter<?>>, IllegalArgumentException> DEFAULT_EXCEPTION_BUILDER = (String errors,
            List<Parameter<?>> parameters) -> new IllegalArgumentException(
                    HelperMessage.getMessage(Constants.DEFAULT_ASSERTION, null, errors, parameters, null));

    private static final BiFunction<String, List<Parameter<?>>, IOException> EXCEPTION_BUILDER = (String errors,
            List<Parameter<?>> parameters) -> new IOException(
                    HelperMessage.getMessage(Constants.DEFAULT_ASSERTION, null, errors, parameters, null));

    /**
     * Test method for {@link PredicateStep#toThrow()}.
     */
    @Test
    public void testToThrow() {
        Expect.exception(() -> {
            Assertor.that("text").isEmpty().toThrow();
            fail();
        }, IllegalArgumentException.class, JUNIT_ERROR);

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
            Assertor.that((CharSequence) null).hasLength(-1).toThrow((Exception) null, true);
            fail();
        }, IllegalArgumentException.class, "the length has to be greater than or equal to 0 and the char sequence cannot be null",
                JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that((CharSequence) null).hasLength(-1).toThrow((Supplier<Throwable>) null);
            fail();
        }, NullPointerException.class, JUNIT_ERROR);

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
        }, IllegalArgumentException.class, "test", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that("text").isEmpty().toThrow("test: '%s*'");
            fail();
        }, IllegalArgumentException.class, "test: 'text'", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that("text").isEmpty().and("").isEmpty().toThrow("%s %2$s test: '%1$s*', '%s*', '%1$s*'.%s*.%2$s*.", "this is", "a");
            fail();
        }, IllegalArgumentException.class, "this is a test: 'text', 'text', 'text'...", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that("text").isEmpty().toThrow(() -> new IOException());
            fail();
        }, IOException.class, JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that((String) null).hasLength(1).toThrow(() -> new IOException());
            fail();
        }, IOException.class, JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that("text").isEmpty().toThrow(new IOException(), false);
            fail();
        }, IOException.class, JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that("text").isEmpty().toThrow(null, false);
            fail();
        }, IllegalArgumentException.class, JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that("text").isEmpty("unused message").toThrow("test");
            fail();
        }, IllegalArgumentException.class, "test", JUNIT_ERROR);

        assertEquals("text", Assertor.that("text").isNotEmpty().toThrow(() -> new IllegalArgumentException()));
        assertEquals("text", Assertor.that("text").isNotEmpty().toThrow(DEFAULT_EXCEPTION_BUILDER));

        Expect.exception(() -> {
            Assertor.that("text").isEmpty().toThrow(DEFAULT_EXCEPTION_BUILDER);
        }, IllegalArgumentException.class, "the char sequence 'text' should be null or empty", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that("text").isEmpty().toThrow(EXCEPTION_BUILDER);
            fail();
        }, IOException.class, "the char sequence 'text' should be null or empty", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that("text").isEmpty().toThrow(() -> new IllegalArgumentException("error"));
        }, IllegalArgumentException.class, "error", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that("text").isEmpty().toThrow(() -> new IOException("error"));
            fail();
        }, IOException.class, "error", JUNIT_ERROR);

        // Check if the checked instance is correctly returned
        final Exception exception = new IllegalArgumentException();
        assertEquals(exception, Assertor.that(exception).isNotNull().toThrow());
    }

    /**
     * Test method for {@link PredicateStep#toThrow()}.
     */
    @Test(expected = ExpectException.class)
    public void testToThrowException() {
        Expect.exception(() -> {
            Assertor.that("text").isNotEmpty().toThrow();
        }, IllegalArgumentException.class);
    }

    /**
     * Test method for {@link PredicateStep#getErrors()}.
     */
    @Test
    public void testGetErrors() {
        assertEquals("", Assertor.that(new BigDecimal("265.45155")).isNotNull(Locale.FRANCE, "test %1$,.3f*").getErrors());

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

    /**
     * Test method for {@link PredicateStep#get()}.
     */
    @Test
    public void testGet() {
        assertFalse(Assertor.that("text").isBlank().get().isPresent());
        Expect.exception(() -> Assertor.that("text").isBlank().get().get(), NoSuchElementException.class, JUNIT_ERROR);
        assertEquals("default", Assertor.that("text").isBlank().result().orElse("default"));

        assertTrue(Assertor.that("text").isNotBlank().get().isPresent());
        assertEquals("text", Assertor.that("text").isNotBlank().get().get());
        assertEquals("text", Assertor.that("text").isNotBlank().get().orElse("default"));

        assertEquals("default", Assertor.that((String) null).isBlank().get().orElse("default"));
        assertEquals("default", Assertor.that((String) null).isNotBlank().get().orElse("default"));

        assertFalse(Assertor.that((CharSequence) null).hasLength(-1).get().isPresent());
    }

    /**
     * Test method for {@link PredicateStep#result()}.
     */
    @Test
    public void testResult() {
        assertFalse(Assertor.that("text").isBlank().result().isPresent());
        Expect.exception(() -> Assertor.that("text").isBlank().result().get(), NoSuchElementException.class, JUNIT_ERROR);
        assertEquals("default", Assertor.that("text").isBlank().result().orElse("default"));

        assertTrue(Assertor.that("text").isNotBlank().result().isPresent());
        assertEquals("text", Assertor.that("text").isNotBlank().result().get());
        assertEquals("text", Assertor.that("text").isNotBlank().result().orElse("default"));

        assertNull(Assertor.that((String) null).isBlank().result().orElse("default"));
        assertEquals("default", Assertor.that((String) null).isNotBlank().result().orElse("default"));

        assertFalse(Assertor.that((CharSequence) null).hasLength(-1).result().isPresent());
    }
}