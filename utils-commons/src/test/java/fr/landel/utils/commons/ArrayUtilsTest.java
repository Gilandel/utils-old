/*-
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Test;

/**
 * Check {@link ArrayUtils}
 *
 * @since 3 août 2016
 * @author Gilles
 *
 */
public class ArrayUtilsTest {

    /**
     * Test method for {@link ArrayUtils#containsAll(T[], U[])}.
     */
    @Test
    public void testContainsAll() {
        Byte[] bytes1 = new Byte[] {'2', '6', null, 'd'};
        Byte[] bytes2 = new Byte[] {'6', null};
        Byte[] bytes3 = new Byte[] {'1', '6', null};
        Byte[] bytes4 = new Byte[] {'1', '3'};

        assertTrue(ArrayUtils.containsAll(bytes1, bytes2));
        assertFalse(ArrayUtils.containsAll(bytes1, bytes3));
        assertFalse(ArrayUtils.containsAll(bytes1, bytes4));

        try {
            ArrayUtils.containsAll(null, bytes2);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }

        try {
            ArrayUtils.containsAll(bytes1, null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link ArrayUtils#containsAny(T[], U[])}.
     */
    @Test
    public void testContainsAny() {
        Byte[] bytes1 = new Byte[] {'2', '6', null, 'd'};
        Byte[] bytes2 = new Byte[] {'6', null};
        Byte[] bytes3 = new Byte[] {'1', '6', null};
        Byte[] bytes4 = new Byte[] {'1', '3'};

        assertTrue(ArrayUtils.containsAny(bytes1, bytes2));
        assertTrue(ArrayUtils.containsAny(bytes1, bytes3));
        assertFalse(ArrayUtils.containsAny(bytes1, bytes4));

        try {
            ArrayUtils.containsAny(null, bytes2);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }

        try {
            ArrayUtils.containsAny(bytes1, null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }
}
