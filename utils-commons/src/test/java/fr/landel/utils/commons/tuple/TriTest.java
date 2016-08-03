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
package fr.landel.utils.commons.tuple;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * Check {@link Tri}
 *
 * @since 2 août 2016
 * @author Gilles
 *
 */
public class TriTest {

    /**
     * Test method for {@link Tri#getLeft()},{@link Tri#getMiddle()},
     * {@link Tri#getRight()}.
     */
    @Test
    public void testGet() {
        String first = "first";
        String second = "second";
        String third = "third";

        Tri<String> tri = Tri.of(first, second, third);

        assertNotNull(tri);
        assertEquals(first, tri.getLeft());
        assertEquals(second, tri.getMiddle());
        assertEquals(third, tri.getRight());
    }

    /**
     * Test method for {@link Tri#equals(Object)}.
     */
    @Test
    public void testEquals() {
        String first = "first";
        String second = "second";
        String third = "third";

        Tri<String> tri = Tri.of(first, second, third);

        assertTrue(tri.equals(tri));
        assertTrue(tri.equals(Tri.of(first, second, third)));
        assertFalse(tri.equals(Tri.of(first, null, third)));
        assertFalse(tri.equals(Tri.of(null, second, third)));
        assertFalse(tri.equals(Tri.of(first, second, null)));
        assertFalse(tri.equals(Tri.of(first, null, null)));
        assertFalse(tri.equals(Tri.of(null, second, null)));
        assertFalse(tri.equals(Tri.of(null, null, third)));
        assertFalse(tri.equals(Tri.of(null, null, null)));
        Tri<String> triNull = null;
        assertFalse(tri.equals(triNull));
    }

    /**
     * Test method for {@link Tri#hashCode()}.
     */
    @Test
    public void testHashCode() {
        String first = "first";
        String second = "second";
        String third = "third";

        Tri<String> tri = Tri.of(first, second, third);

        assertFalse(tri.hashCode() == 0);
        assertNotEquals(first.hashCode(), tri.hashCode());
        assertNotEquals(second.hashCode(), tri.hashCode());
        assertNotEquals(third.hashCode(), tri.hashCode());

        // {@link Arrays#hashCode}
        assertEquals(29_791, Tri.of(null, null, null).hashCode());
    }

    /**
     * Test method for {@link Tri#compareTo(Tri)}.
     */
    @Test
    public void testCompareTo() {
        String first = "first";
        String second = "second";
        String third = "third";

        Tri<String> tri = Tri.of(first, second, third);

        assertEquals(0, tri.compareTo(tri));
        assertEquals(0, tri.compareTo(Tri.of(first, second, third)));
        assertEquals(1, tri.compareTo(Tri.of(first, null, third)));
        assertEquals(1, tri.compareTo(Tri.of(first, second, null)));
        assertEquals(1, tri.compareTo(Tri.of(null, second, third)));
        assertEquals(1, tri.compareTo(Tri.of(first, null, null)));
        assertEquals(1, tri.compareTo(Tri.of(null, second, null)));
        assertEquals(1, tri.compareTo(Tri.of(null, null, third)));
        assertEquals(1, tri.compareTo(Tri.of(null, null, null)));
        assertEquals(Integer.MAX_VALUE, tri.compareTo(null));
    }

    /**
     * Test method for {@link Tri#toString()}.
     */
    @Test
    public void testToString() {
        String first = "first";
        String second = "second";
        String third = "third";

        Tri<String> tri = Tri.of(first, second, third);

        assertEquals("(first, second, third)", tri.toString());
    }

    /**
     * Test method for {@link Tri#toString(java.lang.String)}.
     */
    @Test
    public void testToStringString() {
        String first = "first";
        String second = "second";
        String third = "third";

        Tri<String> tri = Tri.of(first, second, third);

        assertEquals("first, second, third", tri.toString("%s, %s, %s"));
    }

    /**
     * Test method for
     * {@link Tri#ofMutable(java.lang.Object, java.lang.Object)}.
     */
    @Test
    public void testOfMutable() {
        String first = "first";
        String second = "second";
        String third = "third";
        String value = "value";

        MutableTri<String> tri = Tri.ofMutable(first, second, third);

        assertEquals(first, tri.getLeft());
        tri.setLeft(value);
        assertEquals(value, tri.getLeft());

        tri.setMiddle(value);
        assertEquals(value, tri.getMiddle());
        tri.setMiddle(value);
        assertEquals(value, tri.getMiddle());

        tri.setRight(value);
        assertEquals(value, tri.getRight());
        tri.setRight(value);
        assertEquals(value, tri.getRight());
    }
}
