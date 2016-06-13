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

import static org.junit.Assert.fail;

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
     * Test method for
     * {@link Expect#hasNoNullElements(Object[], String, Object...)} .
     */
    @Test
    public void testHasNoNullElementOKsObjectArrayString() {
        try {
            String[] array = new String[] {"1", "3"};
            AssertUtils.check(array).hasNoNullElements();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link Expect#hasNoNullElements(Object[], String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHasNoNullElementsKOObjectArrayString() {
        String[] array = new String[] {null, "2"};
        AssertUtils.check(array).hasNoNullElements();
    }

    /**
     * Test method for {@link Expect#hasNoNullElements(java.lang.Object[])} .
     */
    @Test
    public void testHasNoNullElementsOKObjectArray() {
        try {
            String[] array = new String[] {"1", "2"};
            AssertUtils.check(array).hasNoNullElements("array has null element");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#hasNoNullElements(java.lang.Object[])} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHasNoNullElementsKOObjectArray() {
        String[] array = new String[] {"", null};
        AssertUtils.check(array).hasNoNullElements("array has null element");
    }
}
