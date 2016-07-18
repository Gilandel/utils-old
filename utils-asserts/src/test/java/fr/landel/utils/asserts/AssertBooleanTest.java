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

import static org.junit.Assert.fail;

import org.junit.Test;

/**
 * Check assert boolean
 *
 * @since 18 juil. 2016
 * @author Gilles
 *
 */
public class AssertBooleanTest {

    /**
     * Test method for {@link AssertBoolean#isFalse()} .
     */
    @Test
    public void testIsFalseOKBooleanString() {
        try {
            Assertor.that(false).isFalse().toThrow("not false");
            Assertor.that(false).isFalse().toThrow(new IllegalArgumentException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertBoolean#isFalse()} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsFalseKOBooleanString() {
        Assertor.that(true).isFalse().toThrow("not false");
    }

    /**
     * Test method for {@link AssertBoolean#isFalse()} .
     */
    @Test
    public void testIsFalseOKBoolean() {
        try {
            Assertor.that(false).isFalse();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertBoolean#isFalse()} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsFalseKOBoolean() {
        Assertor.that(true).isFalse().toThrow();
    }

    /**
     * Test method for {@link AssertBoolean#isTrue()} .
     */
    @Test
    public void testIsTrueOKBooleanString() {
        try {
            Assertor.that(true).isTrue().and().isTrue().toThrow("not true");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertBoolean#isTrue()} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsTrueKOBooleanString() {
        Assertor.that(false).isTrue().toThrow("not true");
    }

    /**
     * Test method for {@link AssertBoolean#isTrue()} .
     */
    @Test
    public void testIsTrueOKBoolean() {
        try {
            Assertor.that(true).isTrue();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertBoolean#isTrue()} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsTrueKOBoolean() {
        Assertor.that(false).isTrue().toThrow();
    }
}
