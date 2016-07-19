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
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;

import org.junit.Test;

import fr.landel.utils.assertor.Assertor;
import fr.landel.utils.assertor.Expect;
import fr.landel.utils.assertor.Operator;
import fr.landel.utils.commons.DateUtils;

/**
 * Test operator class
 *
 * @since 17 juil. 2016
 * @author Gilles
 *
 */
public class OperatorTest {

    /**
     * Test method for {@link Operator#and()}.
     */
    @Test
    public void testAnd() {
        final String text = "text";
        assertTrue(Assertor.that(text).isNotEmpty().and().isNotBlank().getResult());

        assertTrue(Assertor.that(text).isNotEmpty().and(true).isTrue().getResult());
        assertFalse(Assertor.that(text).isNotEmpty().and(true).isFalse().getResult());

        assertTrue(Assertor.that(text).isNotEmpty().and(text.getClass()).isAssignableFrom(CharSequence.class).getResult());
        assertFalse(Assertor.that(text).isNotEmpty().and(text.getClass()).isAssignableFrom(StringBuilder.class).getResult());

        assertTrue(Assertor.that(text).isNotEmpty().and(Calendar.getInstance()).isAfter(DateUtils.getCalendar(new Date(0))).getResult());
        assertFalse(Assertor.that(text).isNotEmpty().and(Calendar.getInstance()).isBefore(DateUtils.getCalendar(new Date(0))).getResult());

        assertTrue(Assertor.that(text).isNotEmpty().and(new Date()).isAfter(new Date(0)).getResult());
        assertFalse(Assertor.that(text).isNotEmpty().and(new Date()).isBefore(new Date(0)).getResult());

        assertTrue(Assertor.that(text).isNotEmpty().and(2).isGT(1).getResult());
        assertFalse(Assertor.that(text).isNotEmpty().and(2).isLT(1).getResult());

        assertTrue(Assertor.that(text).isNotEmpty().and("tt").isNotEmpty().getResult());
        assertFalse(Assertor.that(text).isNotEmpty().and("tt").isEmpty().getResult());

        assertTrue(Assertor.that(text).isNotEmpty().and(new String[] {}).isEmpty().getResult());
        assertFalse(Assertor.that(text).isNotEmpty().and(new String[] {}).isNotEmpty().getResult());

        assertTrue(Assertor.that(text).isNotEmpty().and(Collections.emptyList()).isEmpty().getResult());
        assertFalse(Assertor.that(text).isNotEmpty().and(Collections.emptyList()).isNotEmpty().getResult());

        assertTrue(Assertor.that(text).isNotEmpty().and(Collections.emptyMap()).isEmpty().getResult());
        assertFalse(Assertor.that(text).isNotEmpty().and(Collections.emptyMap()).isNotEmpty().getResult());

        assertTrue(Assertor.that(text).isNotEmpty().and((Object) 0).isNotNull().getResult());
        assertFalse(Assertor.that(text).isNotEmpty().and((Object) 0).isNull().getResult());
    }

    /**
     * Test method for {@link Operator#or()}.
     */
    @Test
    public void testOr() {
        final String text = "text";
        assertTrue(Assertor.that(text).isEmpty().or().isNotBlank().getResult());

        assertTrue(Assertor.that(text).isEmpty().or(true).isTrue().getResult());
        assertFalse(Assertor.that(text).isEmpty().or(true).isFalse().getResult());

        assertTrue(Assertor.that(text).isEmpty().or(text.getClass()).isAssignableFrom(CharSequence.class).getResult());
        assertFalse(Assertor.that(text).isEmpty().or(text.getClass()).isAssignableFrom(StringBuilder.class).getResult());

        assertTrue(Assertor.that(text).isEmpty().or(Calendar.getInstance()).isAfter(DateUtils.getCalendar(new Date(0))).getResult());
        assertFalse(Assertor.that(text).isEmpty().or(Calendar.getInstance()).isBefore(DateUtils.getCalendar(new Date(0))).getResult());

        assertTrue(Assertor.that(text).isEmpty().or(new Date()).isAfter(new Date(0)).getResult());
        assertFalse(Assertor.that(text).isEmpty().or(new Date()).isBefore(new Date(0)).getResult());

        assertTrue(Assertor.that(text).isEmpty().or(2).isGT(1).getResult());
        assertFalse(Assertor.that(text).isEmpty().or(2).isLT(1).getResult());

        assertTrue(Assertor.that(text).isEmpty().or("tt").isNotEmpty().getResult());
        assertFalse(Assertor.that(text).isEmpty().or("tt").isEmpty().getResult());

        assertTrue(Assertor.that(text).isEmpty().or(new String[] {}).isEmpty().getResult());
        assertFalse(Assertor.that(text).isEmpty().or(new String[] {}).isNotEmpty().getResult());

        assertTrue(Assertor.that(text).isEmpty().or(Collections.emptyList()).isEmpty().getResult());
        assertFalse(Assertor.that(text).isEmpty().or(Collections.emptyList()).isNotEmpty().getResult());

        assertTrue(Assertor.that(text).isEmpty().or(Collections.emptyMap()).isEmpty().getResult());
        assertFalse(Assertor.that(text).isEmpty().or(Collections.emptyMap()).isNotEmpty().getResult());

        assertTrue(Assertor.that(text).isEmpty().or((Object) 0).isNotNull().getResult());
        assertFalse(Assertor.that(text).isEmpty().or((Object) 0).isNull().getResult());
    }

    /**
     * Test method for {@link Operator#xor()}.
     */
    @Test
    public void testXor() {
        final String text = "text";
        assertTrue(Assertor.that(text).isEmpty().xor().isNotBlank().getResult());

        assertTrue(Assertor.that(text).isEmpty().xor(true).isTrue().getResult());
        assertFalse(Assertor.that(text).isEmpty().xor(true).isFalse().getResult());

        assertTrue(Assertor.that(text).isEmpty().xor(text.getClass()).isAssignableFrom(CharSequence.class).getResult());
        assertFalse(Assertor.that(text).isEmpty().or(text.getClass()).isAssignableFrom(StringBuilder.class).getResult());

        assertTrue(Assertor.that(text).isEmpty().xor(Calendar.getInstance()).isAfter(DateUtils.getCalendar(new Date(0))).getResult());
        assertFalse(Assertor.that(text).isEmpty().xor(Calendar.getInstance()).isBefore(DateUtils.getCalendar(new Date(0))).getResult());

        assertTrue(Assertor.that(text).isEmpty().xor(new Date()).isAfter(new Date(0)).getResult());
        assertFalse(Assertor.that(text).isEmpty().xor(new Date()).isBefore(new Date(0)).getResult());

        assertTrue(Assertor.that(text).isEmpty().xor(2).isGT(1).getResult());
        assertFalse(Assertor.that(text).isEmpty().xor(2).isLT(1).getResult());

        assertTrue(Assertor.that(text).isEmpty().xor("tt").isNotEmpty().getResult());
        assertFalse(Assertor.that(text).isEmpty().xor("tt").isEmpty().getResult());

        assertTrue(Assertor.that(text).isEmpty().xor(new String[] {}).isEmpty().getResult());
        assertFalse(Assertor.that(text).isEmpty().xor(new String[] {}).isNotEmpty().getResult());

        assertTrue(Assertor.that(text).isEmpty().xor(Collections.emptyList()).isEmpty().getResult());
        assertFalse(Assertor.that(text).isEmpty().xor(Collections.emptyList()).isNotEmpty().getResult());

        assertTrue(Assertor.that(text).isEmpty().xor(Collections.emptyMap()).isEmpty().getResult());
        assertFalse(Assertor.that(text).isEmpty().xor(Collections.emptyMap()).isNotEmpty().getResult());

        assertTrue(Assertor.that(text).isEmpty().xor((Object) 0).isNotNull().getResult());
        assertFalse(Assertor.that(text).isEmpty().xor((Object) 0).isNull().getResult());
    }

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
        }, IllegalArgumentException.class, "[Assertion failed] test");

        Expect.exception(() -> {
            Assertor.that("text").isEmpty().toThrow("test: '%p'");
            fail();
        }, IllegalArgumentException.class, "[Assertion failed] test: 'text'");

        Expect.exception(() -> {
            Assertor.that("text").isEmpty().toThrow("%s %2$s test: '%1$p', '%p', '%1$p'.%p.%2$p.", "this is", "a");
            fail();
        }, IllegalArgumentException.class, "[Assertion failed] this is a test: 'text', 'text', 'text'...");

        Expect.exception(() -> {
            Assertor.that("text").isEmpty().toThrow(new IOException());
            fail();
        }, IOException.class);
    }

    /**
     * Test method for {@link Operator#toThrow()}.
     */
    @Test(expected = RuntimeException.class)
    public void testToThrowException() {
        Expect.exception(() -> {
            Assertor.that("text").isNotEmpty().toThrow();
        }, IllegalArgumentException.class);
    }
}
