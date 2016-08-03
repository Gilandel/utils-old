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
 * Check {@link Quadri}
 *
 * @since 2 août 2016
 * @author Gilles
 *
 */
public class QuadriTest {

    /**
     * Test method for {@link Quadri#getLeft()},{@link Quadri#getMiddle()},
     * {@link Quadri#getRight()}.
     */
    @Test
    public void testGet() {
        String first = "first";
        String second = "second";
        String third = "third";
        String fourth = "fourth";

        Quadri<String> quad = Quadri.of(first, second, third, fourth);

        assertNotNull(quad);
        assertEquals(first, quad.getFirst());
        assertEquals(second, quad.getSecond());
        assertEquals(third, quad.getThird());
        assertEquals(fourth, quad.getFourth());
    }

    /**
     * Test method for {@link Quadri#equals(Object)}.
     */
    @Test
    public void testEquals() {
        String first = "first";
        String second = "second";
        String third = "third";
        String fourth = "fourth";

        Quadri<String> quad = Quadri.of(first, second, third, fourth);

        assertTrue(quad.equals(quad));
        assertTrue(quad.equals(Quadri.of(first, second, third, fourth)));
        assertFalse(quad.equals(Quadri.of(first, second, third, 2)));
        assertFalse(quad.equals(Quadri.of(first, null, third, fourth)));
        assertFalse(quad.equals(Quadri.of(null, second, third, fourth)));
        assertFalse(quad.equals(Quadri.of(first, second, null, fourth)));
        assertFalse(quad.equals(Quadri.of(first, null, null, fourth)));
        assertFalse(quad.equals(Quadri.of(null, second, null, fourth)));
        assertFalse(quad.equals(Quadri.of(null, null, third, fourth)));
        assertFalse(quad.equals(Quadri.of(null, null, null, null)));
        Quadri<String> quadNull = null;
        assertFalse(quad.equals(quadNull));
    }

    /**
     * Test method for {@link Quadri#hashCode()}.
     */
    @Test
    public void testHashCode() {
        String first = "first";
        String second = "second";
        String third = "third";
        String fourth = "fourth";

        Quadri<String> quad = Quadri.of(first, second, third, fourth);

        assertFalse(quad.hashCode() == 0);
        assertNotEquals(first.hashCode(), quad.hashCode());
        assertNotEquals(second.hashCode(), quad.hashCode());
        assertNotEquals(third.hashCode(), quad.hashCode());

        // {@link Arrays#hashCode}
        assertEquals(923_521, Quadri.of(null, null, null, null).hashCode());
    }

    /**
     * Test method for {@link Quadri#compareTo(Quadri)}.
     */
    @Test
    public void testCompareTo() {
        String first = "first";
        String second = "second";
        String third = "third";
        String fourth = "fourth";

        Quadri<String> quad = Quadri.of(first, second, third, fourth);

        assertEquals(0, quad.compareTo(quad));
        assertEquals(0, quad.compareTo(Quadri.of(first, second, third, fourth)));
        assertEquals(-1, quad.compareTo(Quadri.of(first, second, third, "fourti")));
        assertEquals(1, quad.compareTo(Quadri.of(first, second, third, "fourtg")));
        assertEquals(1, quad.compareTo(Quadri.of(first, null, third, fourth)));
        assertEquals(1, quad.compareTo(Quadri.of(first, second, null, fourth)));
        assertEquals(1, quad.compareTo(Quadri.of(null, second, third, fourth)));
        assertEquals(1, quad.compareTo(Quadri.of(first, null, null, fourth)));
        assertEquals(1, quad.compareTo(Quadri.of(null, second, null, fourth)));
        assertEquals(1, quad.compareTo(Quadri.of(null, null, third, fourth)));
        assertEquals(1, quad.compareTo(Quadri.of(null, null, null, null)));
        assertEquals(Integer.MAX_VALUE, quad.compareTo(null));
    }

    /**
     * Test method for {@link Quadri#toString()}.
     */
    @Test
    public void testToString() {
        String first = "first";
        String second = "second";
        String third = "third";
        String fourth = "fourth";

        Quadri<String> quad = Quadri.of(first, second, third, fourth);

        assertEquals("(first, second, third, fourth)", quad.toString());
    }

    /**
     * Test method for {@link Quadri#toString(java.lang.String)}.
     */
    @Test
    public void testToStringString() {
        String first = "first";
        String second = "second";
        String third = "third";
        String fourth = "fourth";

        Quadri<String> quad = Quadri.of(first, second, third, fourth);

        assertEquals("first, second, third, fourth", quad.toString("%s, %s, %s, %s"));
    }

    /**
     * Test method for
     * {@link Quadri#ofMutable(java.lang.Object, java.lang.Object)}.
     */
    @Test
    public void testOfMutable() {
        String first = "first";
        String second = "second";
        String third = "third";
        String fourth = "fourth";
        String value = "value";

        MutableQuadri<String> quad = Quadri.ofMutable(first, second, third, fourth);

        assertEquals(first, quad.getFirst());
        quad.setFirst(value);
        assertEquals(value, quad.getFirst());

        assertEquals(second, quad.getSecond());
        quad.setSecond(value);
        assertEquals(value, quad.getSecond());

        assertEquals(third, quad.getThird());
        quad.setThird(value);
        assertEquals(value, quad.getThird());

        assertEquals(fourth, quad.getFourth());
        quad.setFourth(value);
        assertEquals(value, quad.getThird());
    }
}
