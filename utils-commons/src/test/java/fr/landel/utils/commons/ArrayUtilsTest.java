/*-
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Test;

/**
 * Check {@link ArrayUtils}
 *
 * @since Aug 3, 2016
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
        assertFalse(ArrayUtils.containsAll(new Byte[0], bytes2));
        assertFalse(ArrayUtils.containsAll(bytes1, new Character[] {'1'}));
        assertFalse(ArrayUtils.containsAll(bytes1, new Character[] {'1'}, false));

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
        assertFalse(ArrayUtils.containsAny(new Byte[0], bytes2));
        assertFalse(ArrayUtils.containsAny(bytes1, new Character[] {'1'}));
        assertFalse(ArrayUtils.containsAny(bytes1, new Character[] {'1'}, false));

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

    /**
     * Test method for {@link ArrayUtils#count(T[], U[])}.
     */
    @Test
    public void testCountArray() {
        Byte[] bytes1 = new Byte[] {'2', '6', null, 'd'};
        Byte[] bytes2 = new Byte[] {'6', null};
        Byte[] bytes3 = new Byte[] {'1', '6', null};
        Byte[] bytes4 = new Byte[] {'1', '3'};

        assertEquals(2, ArrayUtils.count(bytes1, bytes2));
        assertEquals(2, ArrayUtils.count(bytes1, bytes3));
        assertEquals(0, ArrayUtils.count(bytes1, bytes4));

        assertEquals(0, ArrayUtils.count(new Byte[0], bytes2));

        assertEquals(0, ArrayUtils.count(bytes1, new Character[] {'1'}));

        try {
            ArrayUtils.count(bytes1, null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }

        try {
            ArrayUtils.count(null, bytes1);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link ArrayUtils#count(T[], U)}.
     */
    @Test
    public void testCountObject() {
        Character[] characters = new Character[] {'2', '6', null, '6'};

        assertEquals(2, ArrayUtils.count(characters, '6'));
        assertEquals(2, ArrayUtils.count(characters, '6', false));
        assertEquals(0, ArrayUtils.count(characters, (byte) '6'));
        assertEquals(1, ArrayUtils.count(characters, (Character) null));
        assertEquals(0, ArrayUtils.count(characters, '7'));

        assertEquals(0, ArrayUtils.count(new Character[0], '1'));

        try {
            ArrayUtils.count(null, '1');
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }
}
