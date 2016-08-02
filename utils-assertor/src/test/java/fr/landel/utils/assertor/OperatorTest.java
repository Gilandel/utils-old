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

import java.util.Calendar;
import java.util.Collections;
import java.util.Date;

import org.junit.Test;

import fr.landel.utils.commons.DateUtils;

/**
 * Test operator class
 *
 * @since 17 juil. 2016
 * @author Gilles
 *
 */
public class OperatorTest extends AbstractTest {

    /**
     * Test method for {@link Operator#and()}.
     */
    @Test
    public void testAnd() {
        final String text = "text";
        assertTrue(Assertor.that(text).isNotEmpty().and().isNotBlank().isOK());

        assertTrue(Assertor.that(text).isNotEmpty().and(true).isTrue().isOK());
        assertFalse(Assertor.that(text).isNotEmpty().and(true).isFalse().isOK());

        assertTrue(Assertor.that(text).isNotEmpty().and(text.getClass()).isAssignableFrom(CharSequence.class).isOK());
        assertFalse(Assertor.that(text).isNotEmpty().and(text.getClass()).isAssignableFrom(StringBuilder.class).isOK());

        assertTrue(Assertor.that(text).isNotEmpty().and(Calendar.getInstance()).isAfter(DateUtils.getCalendar(new Date(0))).isOK());
        assertFalse(Assertor.that(text).isNotEmpty().and(Calendar.getInstance()).isBefore(DateUtils.getCalendar(new Date(0))).isOK());

        assertTrue(Assertor.that(text).isNotEmpty().and(new Date()).isAfter(new Date(0)).isOK());
        assertFalse(Assertor.that(text).isNotEmpty().and(new Date()).isBefore(new Date(0)).isOK());

        assertTrue(Assertor.that(text).isNotEmpty().and(2).isGT(1).isOK());
        assertFalse(Assertor.that(text).isNotEmpty().and(2).isLT(1).isOK());

        assertTrue(Assertor.that(text).isNotEmpty().and("tt").isNotEmpty().isOK());
        assertFalse(Assertor.that(text).isNotEmpty().and("tt").isEmpty().isOK());

        assertTrue(Assertor.that(text).isNotEmpty().and(new String[] {}).isEmpty().isOK());
        assertFalse(Assertor.that(text).isNotEmpty().and(new String[] {}).isNotEmpty().isOK());

        assertTrue(Assertor.that(text).isNotEmpty().and(Collections.emptyList()).isEmpty().isOK());
        assertFalse(Assertor.that(text).isNotEmpty().and(Collections.emptyList()).isNotEmpty().isOK());

        assertTrue(Assertor.that(text).isNotEmpty().and(Collections.emptyMap()).isEmpty().isOK());
        assertFalse(Assertor.that(text).isNotEmpty().and(Collections.emptyMap()).isNotEmpty().isOK());

        assertTrue(Assertor.that(text).isNotEmpty().and((Object) 0).isNotNull().isOK());
        assertFalse(Assertor.that(text).isNotEmpty().and((Object) 0).isNull().isOK());
    }

    /**
     * Test method for {@link Operator#or()}.
     */
    @Test
    public void testOr() {
        final String text = "text";
        assertTrue(Assertor.that(text).isEmpty().or().isNotBlank().isOK());

        assertTrue(Assertor.that(text).isEmpty().or(true).isTrue().isOK());
        assertFalse(Assertor.that(text).isEmpty().or(true).isFalse().isOK());

        assertTrue(Assertor.that(text).isEmpty().or(text.getClass()).isAssignableFrom(CharSequence.class).isOK());
        assertFalse(Assertor.that(text).isEmpty().or(text.getClass()).isAssignableFrom(StringBuilder.class).isOK());

        assertTrue(Assertor.that(text).isEmpty().or(Calendar.getInstance()).isAfter(DateUtils.getCalendar(new Date(0))).isOK());
        assertFalse(Assertor.that(text).isEmpty().or(Calendar.getInstance()).isBefore(DateUtils.getCalendar(new Date(0))).isOK());

        assertTrue(Assertor.that(text).isEmpty().or(new Date()).isAfter(new Date(0)).isOK());
        assertFalse(Assertor.that(text).isEmpty().or(new Date()).isBefore(new Date(0)).isOK());

        assertTrue(Assertor.that(text).isEmpty().or(2).isGT(1).isOK());
        assertFalse(Assertor.that(text).isEmpty().or(2).isLT(1).isOK());

        assertTrue(Assertor.that(text).isEmpty().or("tt").isNotEmpty().isOK());
        assertFalse(Assertor.that(text).isEmpty().or("tt").isEmpty().isOK());

        assertTrue(Assertor.that(text).isEmpty().or(new String[] {}).isEmpty().isOK());
        assertFalse(Assertor.that(text).isEmpty().or(new String[] {}).isNotEmpty().isOK());

        assertTrue(Assertor.that(text).isEmpty().or(Collections.emptyList()).isEmpty().isOK());
        assertFalse(Assertor.that(text).isEmpty().or(Collections.emptyList()).isNotEmpty().isOK());

        assertTrue(Assertor.that(text).isEmpty().or(Collections.emptyMap()).isEmpty().isOK());
        assertFalse(Assertor.that(text).isEmpty().or(Collections.emptyMap()).isNotEmpty().isOK());

        assertTrue(Assertor.that(text).isEmpty().or((Object) 0).isNotNull().isOK());
        assertFalse(Assertor.that(text).isEmpty().or((Object) 0).isNull().isOK());
    }

    /**
     * Test method for {@link Operator#xor()}.
     */
    @Test
    public void testXor() {
        final String text = "text";
        assertTrue(Assertor.that(text).isEmpty().xor().isNotBlank().isOK());

        assertTrue(Assertor.that(text).isEmpty().xor(true).isTrue().isOK());
        assertFalse(Assertor.that(text).isEmpty().xor(true).isFalse().isOK());

        assertTrue(Assertor.that(text).isEmpty().xor(text.getClass()).isAssignableFrom(CharSequence.class).isOK());
        assertFalse(Assertor.that(text).isEmpty().or(text.getClass()).isAssignableFrom(StringBuilder.class).isOK());

        assertTrue(Assertor.that(text).isEmpty().xor(Calendar.getInstance()).isAfter(DateUtils.getCalendar(new Date(0))).isOK());
        assertFalse(Assertor.that(text).isEmpty().xor(Calendar.getInstance()).isBefore(DateUtils.getCalendar(new Date(0))).isOK());

        assertTrue(Assertor.that(text).isEmpty().xor(new Date()).isAfter(new Date(0)).isOK());
        assertFalse(Assertor.that(text).isEmpty().xor(new Date()).isBefore(new Date(0)).isOK());

        assertTrue(Assertor.that(text).isEmpty().xor(2).isGT(1).isOK());
        assertFalse(Assertor.that(text).isEmpty().xor(2).isLT(1).isOK());

        assertTrue(Assertor.that(text).isEmpty().xor("tt").isNotEmpty().isOK());
        assertFalse(Assertor.that(text).isEmpty().xor("tt").isEmpty().isOK());

        assertTrue(Assertor.that(text).isEmpty().xor(new String[] {}).isEmpty().isOK());
        assertFalse(Assertor.that(text).isEmpty().xor(new String[] {}).isNotEmpty().isOK());

        assertTrue(Assertor.that(text).isEmpty().xor(Collections.emptyList()).isEmpty().isOK());
        assertFalse(Assertor.that(text).isEmpty().xor(Collections.emptyList()).isNotEmpty().isOK());

        assertTrue(Assertor.that(text).isEmpty().xor(Collections.emptyMap()).isEmpty().isOK());
        assertFalse(Assertor.that(text).isEmpty().xor(Collections.emptyMap()).isNotEmpty().isOK());

        assertTrue(Assertor.that(text).isEmpty().xor((Object) 0).isNotNull().isOK());
        assertFalse(Assertor.that(text).isEmpty().xor((Object) 0).isNull().isOK());
    }
}
