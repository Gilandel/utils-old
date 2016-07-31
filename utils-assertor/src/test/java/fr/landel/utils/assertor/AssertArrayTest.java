/*
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

import org.junit.Test;

import fr.landel.utils.assertor.expect.Expect;

/**
 * Check {@link AssertArray}
 *
 * @since 10 dec. 2015
 * @author Gilles Landel
 *
 */
public class AssertArrayTest extends AbstractTest {

    /**
     * Test method for {@link AssertArray#hasSize} .
     */
    @Test
    public void testHasSize() {
        String[] array = new String[] {null, "2"};
        assertTrue(Assertor.that(array).hasLength(2).isOK());
        assertFalse(Assertor.that(array).hasLength(1).isOK());
        assertFalse(Assertor.that((String[]) null).hasLength(1).isOK());
        assertFalse(Assertor.that(array).hasLength(-1).isOK());
    }

    /**
     * Test method for {@link AssertArray#hasNotSize} .
     */
    @Test
    public void testHasNotSize() {
        String[] array = new String[] {null, "2"};
        assertTrue(Assertor.that(array).not().hasLength(1).isOK());
        assertFalse(Assertor.that(array).not().hasLength(2).isOK());
        assertFalse(Assertor.that((String[]) null).not().hasLength(1).isOK());
        assertFalse(Assertor.that(array).not().hasLength(-1).isOK());
    }

    /**
     * Test method for {@link AssertArray#isEmpty()} .
     */
    @Test
    public void testIsEmpty() {
        assertFalse(Assertor.that(new String[] {null, "2"}).isEmpty().isOK());
        assertTrue(Assertor.that(new String[] {}).isEmpty().isOK());
        assertTrue(Assertor.that((String[]) null).isEmpty().isOK());
    }

    /**
     * Test method for {@link AssertArray#isNotEmpty} .
     */
    @Test
    public void testIsNotEmpty() {
        try {
            Assertor.that(new String[] {""}).isNotEmpty().toThrow("empty array");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }

        Expect.exception(() -> {
            Assertor.that(new Object[0]).isNotEmpty().toThrow("empty array");
            fail();
        }, IllegalArgumentException.class, "empty array");
    }

    /**
     * Test method for {@link AssertArray#containsNull} .
     */
    @Test
    public void testContainsNull() {
        try {
            String[] array = new String[] {null, "2"};
            Assertor.that(array).contains(null).toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }

        Expect.exception(() -> {
            Assertor.that((Object[]) null).contains(null).toThrow();
            fail();
        }, IllegalArgumentException.class, "the array cannot be null", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that(new String[] {"1", "3"}).contains(null).toThrow("array hasn't null element");
            fail();
        }, IllegalArgumentException.class, "array hasn't null element");
    }

    /**
     * Test method for {@link AssertArray#doesNotContainNull} .
     */
    @Test
    public void testDoesNotContainNull() {
        try {
            String[] array = new String[] {"1", "2"};
            Assertor.that(array).not().contains(null).toThrow("array has null element");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }

        Expect.exception(() -> {
            Assertor.that((Object[]) null).not().contains(null).toThrow("array has null element");
            fail();
        }, IllegalArgumentException.class, "array has null element");

        Expect.exception(() -> {
            Assertor.that(new String[] {"", null}).not().contains(null).toThrow("array has null element");
            fail();
        }, IllegalArgumentException.class, "array has null element");
    }

    /**
     * Test method for {@link AssertArray#contains} .
     */
    @Test
    public void testContains() {
        assertTrue(Assertor.that(new String[] {null, "2"}).contains("2").isOK());
        assertFalse(Assertor.that(new String[] {}).contains((String) null).isOK());
        assertFalse(Assertor.that((String[]) null).contains("").isOK());

        assertTrue(Assertor.that(new String[] {null, "2", "3"}).containsAll(new String[] {null, "3"}).isOK());
        assertFalse(Assertor.that(new String[] {}).containsAll((String[]) null).isOK());
        assertFalse(Assertor.that((String[]) null).containsAll(new String[] {null, "3"}).isOK());
    }

    /**
     * Test method for {@link AssertArray#contains} .
     */
    @Test
    public void testDoesNotContain() {
        assertFalse(Assertor.that(new String[] {null, "2"}).not().contains("2").isOK());
        assertTrue(Assertor.that(new String[] {}).not().contains((String) null).isOK());
        assertFalse(Assertor.that((String[]) null).not().contains("").isOK());

        assertFalse(Assertor.that(new String[] {null, "2", "3"}).not().containsAll(new String[] {null, "3"}).isOK());
        assertFalse(Assertor.that(new String[] {}).not().containsAll((String[]) null).isOK());
        assertFalse(Assertor.that((String[]) null).not().containsAll(new String[] {null, "3"}).isOK());
    }
}