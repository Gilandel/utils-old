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

import org.junit.Test;

import fr.landel.utils.assertor.expect.Expect;

/**
 * Check the assert number classes
 *
 * @since 16 juil. 2016
 * @author Gilles
 *
 */
public class AssertorNumberTest extends AbstractTest {

    /**
     * Test method for {@link AssertNumber#isEqual(java.lang.Number)}.
     */
    @Test
    public void testIsEqualN() {
        assertTrue(Assertor.that(2).isEqual(2).isOK());
        assertFalse(Assertor.that(2).isEqual(1).isOK());
        assertFalse(Assertor.that(2).isEqual(null).isOK());
        assertFalse(Assertor.that((Integer) null).isEqual(1).isOK());
        assertTrue(Assertor.that((Integer) null).isEqual(null).isOK());

        assertTrue(Assertor.that(2).isEqual(2, "error1").isOK());
        assertFalse(Assertor.that(2).isEqual(1, "error1").isOK());
        assertFalse(Assertor.that(2).isEqual(null, "error1").isOK());
        assertFalse(Assertor.that((Integer) null).isEqual(1, "error1").isOK());
        assertTrue(Assertor.that((Integer) null).isEqual(null, "error1").isOK());

        assertTrue(Assertor.that(2).isEqual(2, "error1").and().isNotNull().isOK());
        assertTrue(Assertor.that(2).isEqual(2, "error1").or().isNull().isOK());
        assertTrue(Assertor.that(2).isEqual(2, "error1").xor().isNull().isOK());

        assertEquals("error1", Assertor.that(2).isEqual(1, "error1").getErrors());

        Expect.exception(() -> {
            Assertor.that(2).isEqual(1, "error1 %1$s* %s %2$s*", 0).toThrow();
        }, IllegalArgumentException.class, "error1 2 0 1");

        Expect.exception(() -> {
            Assertor.that(2).isEqual(1, "error1").toThrow("error2");
        }, IllegalArgumentException.class, "error2");
    }

    /**
     * Test method for {@link AssertNumber#isNotEqual(java.lang.Number)}.
     */
    @Test
    public void testIsNotEqualN() {
        assertTrue(Assertor.that(2).isNotEqual(3).isOK());
        assertFalse(Assertor.that(2).isNotEqual(2).isOK());
        assertTrue(Assertor.that(2).isNotEqual(null).isOK());
        assertTrue(Assertor.that((Integer) null).isNotEqual(1).isOK());

        assertTrue(Assertor.that(2).isNotEqual(3, "error1").isOK());
        assertFalse(Assertor.that(2).isNotEqual(2, "error1").isOK());
        assertTrue(Assertor.that(2).isNotEqual(null, "error1").isOK());
        assertTrue(Assertor.that((Integer) null).isNotEqual(1, "error1").isOK());

        assertEquals("error1", Assertor.that(2).isNotEqual(2, "error1").getErrors());

        Expect.exception(() -> {
            Assertor.that(2).isNotEqual(2, "error1 %1$s* %s", 0).toThrow();
        }, IllegalArgumentException.class, "error1 2 0");

        Expect.exception(() -> {
            Assertor.that(2).isNotEqual(2, "error1").toThrow("error2");
        }, IllegalArgumentException.class, "error2");
    }

    /**
     * Test method for {@link AssertNumber#isGT(java.lang.Number)}.
     */
    @Test
    public void testIsGT() {
        assertTrue(Assertor.that(2).isGT(1).isOK());
        assertFalse(Assertor.that(2).isGT(2).isOK());
        assertFalse(Assertor.that(2).isGT(3).isOK());
        assertTrue(Assertor.that(2).isGT(null).isOK());
        assertFalse(Assertor.that((Integer) null).isGT(1).isOK());

        assertTrue(Assertor.that(2).isGT(1, "error1").isOK());
        assertFalse(Assertor.that(2).isGT(2, "error1").isOK());
        assertFalse(Assertor.that(2).isGT(3, "error1").isOK());
        assertTrue(Assertor.that(2).isGT(null, "error1").isOK());
        assertFalse(Assertor.that((Integer) null).isGT(1, "error1").isOK());

        assertEquals("error1", Assertor.that(2).isGT(2, "error1").getErrors());

        Expect.exception(() -> {
            Assertor.that(2).isGT(2, "error1 %1$s* %s", 0).toThrow();
        }, IllegalArgumentException.class, "error1 2 0");

        Expect.exception(() -> {
            Assertor.that(2).isGT(2, "error1").toThrow("error2");
        }, IllegalArgumentException.class, "error2");
    }

    /**
     * Test method for {@link AssertNumber#isGTE(java.lang.Number)}.
     */
    @Test
    public void testIsGTE() {
        assertTrue(Assertor.that(2).isGTE(1).isOK());
        assertTrue(Assertor.that(2).isGTE(2).isOK());
        assertFalse(Assertor.that(2).isGTE(3).isOK());
        assertTrue(Assertor.that(2).isGTE(null).isOK());
        assertFalse(Assertor.that((Integer) null).isGTE(1).isOK());

        assertTrue(Assertor.that(2).isGTE(1, "error1").isOK());
        assertTrue(Assertor.that(2).isGTE(2, "error1").isOK());
        assertFalse(Assertor.that(2).isGTE(3, "error1").isOK());
        assertTrue(Assertor.that(2).isGTE(null, "error1").isOK());
        assertFalse(Assertor.that((Integer) null).isGTE(1, "error1").isOK());

        assertEquals("error1", Assertor.that(2).isGTE(3, "error1").getErrors());

        Expect.exception(() -> {
            Assertor.that(2).isGTE(3, "error1 %1$s* %s", 0).toThrow();
        }, IllegalArgumentException.class, "error1 2 0");

        Expect.exception(() -> {
            Assertor.that(2).isGTE(3, "error1").toThrow("error2");
        }, IllegalArgumentException.class, "error2");
    }

    /**
     * Test method for {@link AssertNumber#isLT(java.lang.Number)}.
     */
    @Test
    public void testIsLT() {
        assertTrue(Assertor.that(1).isLT(2).isOK());
        assertFalse(Assertor.that(2).isLT(2).isOK());
        assertFalse(Assertor.that(1).isLT(0).isOK());
        assertFalse(Assertor.that(2).isLT(null).isOK());
        assertTrue(Assertor.that((Integer) null).isLT(1).isOK());

        assertTrue(Assertor.that(1).isLT(2, "error1").isOK());
        assertFalse(Assertor.that(2).isLT(2, "error1").isOK());
        assertFalse(Assertor.that(1).isLT(0, "error1").isOK());
        assertFalse(Assertor.that(2).isLT(null, "error1").isOK());
        assertTrue(Assertor.that((Integer) null).isLT(1, "error1").isOK());
        assertEquals("error1", Assertor.that(2).isLT(1, "error1").getErrors());

        Expect.exception(() -> {
            Assertor.that(2).isLT(1, "error1 %1$s* %s", 0).toThrow();
        }, IllegalArgumentException.class, "error1 2 0");

        Expect.exception(() -> {
            Assertor.that(2).isLT(1, "error1").toThrow("error2");
        }, IllegalArgumentException.class, "error2");
    }

    /**
     * Test method for {@link AssertNumber#isLTE(java.lang.Number)}.
     */
    @Test
    public void testIsLTE() {
        assertTrue(Assertor.that(1).isLTE(2).isOK());
        assertTrue(Assertor.that(2).isLTE(2).isOK());
        assertFalse(Assertor.that(1).isLTE(0).isOK());
        assertFalse(Assertor.that(2).isLTE(null).isOK());
        assertTrue(Assertor.that((Integer) null).isLTE(1).isOK());

        assertTrue(Assertor.that(1).isLTE(2, "error1").isOK());
        assertTrue(Assertor.that(2).isLTE(2, "error1").isOK());
        assertFalse(Assertor.that(1).isLTE(0, "error1").isOK());
        assertFalse(Assertor.that(2).isLTE(null, "error1").isOK());
        assertTrue(Assertor.that((Integer) null).isLTE(1, "error1").isOK());
        assertEquals("error1", Assertor.that(2).isLTE(1, "error1").getErrors());

        Expect.exception(() -> {
            Assertor.that(2).isLTE(1, "error1 %1$s* %s", 0).toThrow();
        }, IllegalArgumentException.class, "error1 2 0");

        Expect.exception(() -> {
            Assertor.that(2).isLTE(1, "error1").toThrow("error2");
        }, IllegalArgumentException.class, "error2");
    }
}
