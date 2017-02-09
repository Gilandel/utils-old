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
package fr.landel.utils.commons.tuple;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * Check {@link Quad}
 *
 * @since Aug 2, 2016
 * @author Gilles
 *
 */
public class QuadTest {

    /**
     * Test method for {@link Quad#getLeft()},{@link Quad#getMiddle()},
     * {@link Quad#getRight()}.
     */
    @Test
    public void testGet() {
        String first = "first";
        String second = "second";
        String third = "third";
        Integer fourth = 2;

        Quad<String, String, String, Integer> quad = Quad.of(first, second, third, fourth);

        assertNotNull(quad);
        assertEquals(first, quad.getFirst());
        assertEquals(second, quad.getSecond());
        assertEquals(third, quad.getThird());
        assertEquals(fourth, quad.getFourth());
    }

    /**
     * Test method for {@link Quad#equals(Object)}.
     */
    @Test
    public void testEquals() {
        String first = "first";
        String second = "second";
        String third = "third";
        Integer fourth = 2;

        Quad<String, String, String, Integer> quad = Quad.of(first, second, third, fourth);

        assertTrue(quad.equals(quad));
        assertTrue(quad.equals(Quad.of(first, second, third, 2)));
        assertFalse(quad.equals(Quad.of(first, second, third, 3)));
        assertFalse(quad.equals(Quad.of(first, null, third, 2)));
        assertFalse(quad.equals(Quad.of(null, second, third, 2)));
        assertFalse(quad.equals(Quad.of(first, second, null, 2)));
        assertFalse(quad.equals(Quad.of(first, null, null, 2)));
        assertFalse(quad.equals(Quad.of(null, second, null, 2)));
        assertFalse(quad.equals(Quad.of(null, null, third, 2)));
        assertFalse(quad.equals(Quad.of(null, null, null, null)));
        Quad<String, String, String, Integer> quadNull = null;
        assertFalse(quad.equals(quadNull));
    }

    /**
     * Test method for {@link Quad#hashCode()}.
     */
    @Test
    public void testHashCode() {
        String first = "first";
        String second = "second";
        String third = "third";
        Integer fourth = 2;

        Quad<String, String, String, Integer> quad = Quad.of(first, second, third, fourth);

        assertFalse(quad.hashCode() == 0);
        assertNotEquals(first.hashCode(), quad.hashCode());
        assertNotEquals(second.hashCode(), quad.hashCode());
        assertNotEquals(third.hashCode(), quad.hashCode());

        // {@link Arrays#hashCode}
        assertEquals(923_521, Quad.of(null, null, null, null).hashCode());
    }

    /**
     * Test method for {@link Quad#compareTo(Quad)}.
     */
    @Test
    public void testCompareTo() {
        String first = "first";
        String second = "second";
        String third = "third";
        Integer fourth = 2;

        Quad<String, String, String, Integer> quad = Quad.of(first, second, third, fourth);

        assertEquals(0, quad.compareTo(quad));
        assertEquals(0, quad.compareTo(Quad.of(first, second, third, 2)));
        assertEquals(-1, quad.compareTo(Quad.of(first, second, third, 3)));
        assertEquals(1, quad.compareTo(Quad.of(first, second, third, 1)));
        assertEquals(1, quad.compareTo(Quad.of(first, null, third, 1)));
        assertEquals(1, quad.compareTo(Quad.of(first, second, null, 1)));
        assertEquals(1, quad.compareTo(Quad.of(null, second, third, 1)));
        assertEquals(1, quad.compareTo(Quad.of(first, null, null, 1)));
        assertEquals(1, quad.compareTo(Quad.of(null, second, null, 1)));
        assertEquals(1, quad.compareTo(Quad.of(null, null, third, 1)));
        assertEquals(1, quad.compareTo(Quad.of(null, null, null, null)));
        assertEquals(Integer.MAX_VALUE, quad.compareTo(null));
    }

    /**
     * Test method for {@link Quad#toString()}.
     */
    @Test
    public void testToString() {
        String first = "first";
        String second = "second";
        String third = "third";
        Integer fourth = 2;

        Quad<String, String, String, Integer> quad = Quad.of(first, second, third, fourth);

        assertEquals("(first,second,third,2)", quad.toString());
    }

    /**
     * Test method for {@link Quad#toString(java.lang.String)}.
     */
    @Test
    public void testToStringString() {
        String first = "first";
        String second = "second";
        String third = "third";
        Integer fourth = 2;

        Quad<String, String, String, Integer> quad = Quad.of(first, second, third, fourth);

        assertEquals("first, second, third, 2", quad.toString("%s, %s, %s, %s"));
    }

    /**
     * Test method for
     * {@link Quad#ofMutable(java.lang.Object, java.lang.Object)}.
     */
    @Test
    public void testOfMutable() {
        String first = "first";
        String second = "second";
        String third = "third";
        String value = "value";

        MutableQuad<String, String, String, Integer> quad = Quad.ofMutable(first, second, third, 2);

        assertEquals(first, quad.getFirst());
        quad.setFirst(value);
        assertEquals(value, quad.getFirst());

        assertEquals(second, quad.getSecond());
        quad.setSecond(value);
        assertEquals(value, quad.getSecond());

        assertEquals(third, quad.getThird());
        quad.setThird(value);
        assertEquals(value, quad.getThird());

        assertEquals(2, quad.getFourth().intValue());
        quad.setFourth(1);
        assertEquals(1, quad.getFourth().intValue());
    }
}
