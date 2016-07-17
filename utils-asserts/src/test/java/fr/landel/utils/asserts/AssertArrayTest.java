/*
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
import static org.junit.Assert.fail;

import java.util.Arrays;
import java.util.Collections;

import org.junit.Test;

/**
 * Check {@link AssertArray}
 *
 * @since 10 dec. 2015
 * @author Gilles Landel
 *
 */
public class AssertArrayTest {

    /**
     * Test method for {@link AssertArray#hasSize()} .
     */
    @Test
    public void testHasSize() {
        String[] array = new String[] {null, "2"};
        assertTrue(Assertor.that(array).hasSize(2).getResult());
        assertFalse(Assertor.that(array).hasSize(1).getResult());
        assertFalse(Assertor.that((String[]) null).hasSize(1).getResult());
        assertFalse(Assertor.that(array).hasSize(-1).getResult());
    }

    /**
     * Test method for {@link AssertArray#hasNotSize()} .
     */
    @Test
    public void testHasNotSize() {
        String[] array = new String[] {null, "2"};
        assertTrue(Assertor.that(array).hasNotSize(1).getResult());
        assertFalse(Assertor.that(array).hasNotSize(2).getResult());
        assertFalse(Assertor.that((String[]) null).hasNotSize(1).getResult());
        assertFalse(Assertor.that(array).hasNotSize(-1).getResult());
    }

    /**
     * Test method for {@link AssertArray#isEmpty()} .
     */
    @Test
    public void testIsEmpty() {
        assertFalse(Assertor.that(new String[] {null, "2"}).isEmpty().getResult());
        assertTrue(Assertor.that(new String[] {}).isEmpty().getResult());
        assertTrue(Assertor.that((String[]) null).isEmpty().getResult());
    }

    /**
     * Test method for {@link AssertArray#containsNull()} .
     */
    @Test
    public void testContainsNullOKsObjectArrayString() {
        try {
            String[] array = new String[] {null, "2"};
            Assertor.that(array).containsNull().toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertArray#containsNull()} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testContainsNullKOObjectArrayString() {
        String[] array = new String[] {"1", "3"};
        Assertor.that(array).containsNull().toThrow();
    }

    /**
     * Test method for {@link AssertArray#doesNotContainNull()} .
     */
    @Test
    public void testDoesNotContainNullOKObjectArray() {
        try {
            String[] array = new String[] {"1", "2"};
            Assertor.that(array).doesNotContainNull().toThrow("array has null element");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertArray#doesNotContainNull()} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testDoesNotContainNullKOObjectArray() {
        String[] array = new String[] {"", null};
        Assertor.that(array).doesNotContainNull().toThrow("array has null element");
    }

    /**
     * Test method for {@link AssertArray#contains()} .
     */
    @Test
    public void testContains() {
        assertTrue(Assertor.that(new String[] {null, "2"}).contains("2").getResult());
        assertFalse(Assertor.that(new String[] {}).contains((String) null).getResult());
        assertFalse(Assertor.that((String[]) null).contains("").getResult());

        assertTrue(Assertor.that(new String[] {null, "2", "3"}).contains(new String[] {null, "3"}).getResult());
        assertTrue(Assertor.that(new String[] {}).contains((String[]) null).getResult());
        assertTrue(Assertor.that((String[]) null).contains(new String[] {null, "3"}).getResult());
    }

    /**
     * Test method for {@link AssertArray#doesNotContain()} .
     */
    @Test
    public void testDoesNotContain() {
        assertFalse(Assertor.that(new String[] {null, "2"}).doesNotContain("2").getResult());
        assertTrue(Assertor.that(new String[] {}).doesNotContain((String) null).getResult());
        assertTrue(Assertor.that((String[]) null).doesNotContain("").getResult());

        assertFalse(Assertor.that(new String[] {null, "2", "3"}).doesNotContain(new String[] {null, "3"}).getResult());
        assertFalse(Assertor.that(new String[] {}).doesNotContain((String[]) null).getResult());
        assertFalse(Assertor.that((String[]) null).doesNotContain(new String[] {null, "3"}).getResult());
    }

    /**
     * Test method for
     * {@link AssertArray#isNotEmpty(Object[], String, Object...)} .
     */
    @Test
    public void testIsNotEmptyOKObjectArrayString() {
        try {
            Assertor.that(Arrays.asList("").toArray()).isNotEmpty().toThrow("empty array");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link AssertArray#isNotEmpty(Object[], String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOObjectArrayString() {
        Assertor.that(Collections.emptyList().toArray()).isNotEmpty().toThrow("empty array");
    }

    /**
     * Test method for {@link AssertArray#isNotEmpty(java.lang.Object[])} .
     */
    @Test
    public void testIsNotEmptyOKObjectArray() {
        try {
            Assertor.that(Arrays.asList("").toArray()).isNotEmpty().toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertArray#isNotEmpty(java.lang.Object[])} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOObjectArray() {
        Assertor.that(Collections.emptyList().toArray()).isNotEmpty().toThrow();
    }
}