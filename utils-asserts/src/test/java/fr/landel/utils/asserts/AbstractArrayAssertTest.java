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
 * Check assert
 *
 * @since 10 dec. 2015
 * @author Gilles Landel
 *
 */
public class AbstractArrayAssertTest {

    /**
     * Test method for
     * {@link AssertUtils#hasNoNullElements(Object[], String, Object...)} .
     */
    @Test
    public void testHasNoNullElementOKsObjectArrayString() {
        try {
            String[] array = new String[] {"1", "3"};
            AssertUtils.hasNoNullElements(array);
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link AssertUtils#hasNoNullElements(Object[], String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHasNoNullElementsKOObjectArrayString() {
        String[] array = new String[] {null, "2"};
        AssertUtils.hasNoNullElements(array);
    }

    /**
     * Test method for {@link AssertUtils#hasNoNullElements(java.lang.Object[])}
     * .
     */
    @Test
    public void testHasNoNullElementsOKObjectArray() {
        try {
            String[] array = new String[] {"1", "2"};
            AssertUtils.hasNoNullElements(array, "array has null element");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertUtils#hasNoNullElements(java.lang.Object[])}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHasNoNullElementsKOObjectArray() {
        String[] array = new String[] {"", null};
        AssertUtils.hasNoNullElements(array, "array has null element");
    }
}
