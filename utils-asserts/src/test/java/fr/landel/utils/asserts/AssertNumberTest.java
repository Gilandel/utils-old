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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * Check the assert number classes
 *
 * @since 16 juil. 2016
 * @author Gilles
 *
 */
public class AssertNumberTest {

    /**
     * Test method for {@link AssertNumber#isEqual(java.lang.Number)}.
     */
    @Test
    public void testIsEqualN() {
        assertTrue(Assertor.that(2).isEqual(2).getResult());
        assertFalse(Assertor.that(2).isEqual(1).getResult());
        assertFalse(Assertor.that(2).isEqual(null).getResult());
        assertFalse(Assertor.that((Integer) null).isEqual(1).getResult());
        assertTrue(Assertor.that((Integer) null).isEqual(null).getResult());
    }

    /**
     * Test method for {@link AssertNumber#isNotEqual(java.lang.Number)}.
     */
    @Test
    public void testIsNotEqualN() {
        assertTrue(Assertor.that(2).isNotEqual(3).getResult());
        assertFalse(Assertor.that(2).isNotEqual(2).getResult());
        assertTrue(Assertor.that(2).isNotEqual(null).getResult());
        assertTrue(Assertor.that((Integer) null).isNotEqual(1).getResult());
    }

    /**
     * Test method for {@link AssertNumber#isGT(java.lang.Number)}.
     */
    @Test
    public void testIsGT() {
        assertTrue(Assertor.that(2).isGT(1).getResult());
        assertFalse(Assertor.that(2).isGT(2).getResult());
        assertFalse(Assertor.that(2).isGT(3).getResult());
        assertTrue(Assertor.that(2).isGT(null).getResult());
        assertFalse(Assertor.that((Integer) null).isGT(1).getResult());
    }

    /**
     * Test method for {@link AssertNumber#isGTE(java.lang.Number)}.
     */
    @Test
    public void testIsGTE() {
        assertTrue(Assertor.that(2).isGTE(1).getResult());
        assertTrue(Assertor.that(2).isGTE(2).getResult());
        assertFalse(Assertor.that(2).isGTE(3).getResult());
        assertTrue(Assertor.that(2).isGTE(null).getResult());
        assertFalse(Assertor.that((Integer) null).isGTE(1).getResult());
    }

    /**
     * Test method for {@link AssertNumber#isLT(java.lang.Number)}.
     */
    @Test
    public void testIsLT() {
        assertTrue(Assertor.that(1).isLT(2).getResult());
        assertFalse(Assertor.that(2).isLT(2).getResult());
        assertFalse(Assertor.that(1).isLT(0).getResult());
        assertFalse(Assertor.that(2).isLT(null).getResult());
        assertTrue(Assertor.that((Integer) null).isLT(1).getResult());
    }

    /**
     * Test method for {@link AssertNumber#isLTE(java.lang.Number)}.
     */
    @Test
    public void testIsLTE() {
        assertTrue(Assertor.that(1).isLTE(2).getResult());
        assertTrue(Assertor.that(2).isLTE(2).getResult());
        assertFalse(Assertor.that(1).isLTE(0).getResult());
        assertFalse(Assertor.that(2).isLTE(null).getResult());
        assertTrue(Assertor.that((Integer) null).isLTE(1).getResult());
    }
}
