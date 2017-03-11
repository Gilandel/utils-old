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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.Field;

import org.junit.Test;

/**
 * Check the generic class
 *
 * @since Jul 27, 2016
 * @author Gilles
 *
 */
public class GenericTest {

    /**
     * Test method for {@link Generic#hashCode()}.
     */
    @Test
    public void testHashCode() {
        assertEquals(31 * 31, Generic.of(null, null).hashCode());
        assertTrue(Generic.of(null, 1).hashCode() != 0);
    }

    /**
     * Test method for {@link Generic#get(int)}.
     */
    @Test()
    public void testGet() {
        assertNull(Generic.of((Object) null).get(0));
        assertEquals("test", Generic.of("test", 1).get(0));
        assertEquals(1, Generic.of("test", 1).get(1));
    }

    /**
     * Test method for {@link Generic#get(int)}.
     */
    @Test(expected = IndexOutOfBoundsException.class)
    public void testGetError1() {
        Generic.of("test", 1).get(2);
    }

    /**
     * Test method for {@link Generic#get(int)}.
     */
    @Test(expected = IndexOutOfBoundsException.class)
    public void testGetError2() {
        Generic.of("test", 1).get(-1);
    }

    /**
     * Test method for {@link Generic#get(int)}.
     */
    @Test(expected = IndexOutOfBoundsException.class)
    public void testGetEmpty() {
        Generic.of().get(0);
    }

    /**
     * Test method for {@link Generic#getList()}.
     */
    @Test
    public void testGetList() {
        assertNotNull(Generic.of(3, 6).getList());
        assertEquals(2, Generic.of(3, 6).size());
        assertEquals(2, Generic.of(3, 6).getList().size());
    }

    /**
     * Test method for {@link Generic#getList()}.
     */
    @Test(expected = UnsupportedOperationException.class)
    public void testGetListError() {
        Generic.of(3, 6).getList().add(3);
    }

    /**
     * Test method for {@link Generic#compareTo(Generic)}.
     */
    @Test
    public void testCompareTo() {
        assertEquals(0, Generic.of(2, 3).compareTo(Generic.of(2, 3)));
        assertEquals(0, Generic.of(2, 3, null, "abc").compareTo(Generic.of(2, 3, null, "abc")));
        assertEquals(1, Generic.of(2, 3, "abd").compareTo(Generic.of(2, 3, "abc")));
        assertEquals(-1, Generic.of(2, 3).compareTo(Generic.of(2, 4)));
        assertEquals(Integer.MAX_VALUE, Generic.of(2, 3).compareTo(null));
        assertEquals(Integer.MAX_VALUE, Generic.of(2, 3).compareTo(Generic.of(1)));
        assertEquals(Integer.MIN_VALUE, Generic.of(2, 3).compareTo(Generic.of(2, 3, 5)));

        assertEquals(0, Generic.of().compareTo(Generic.of()));

        ImmutableGeneric<Integer> gen = Generic.of(2);
        assertEquals(0, gen.compareTo(gen));
    }

    /**
     * Test method for {@link Generic#compareTo(Generic)}.
     */
    @Test(expected = ClassCastException.class)
    public void testCompareToError() {
        Generic.of(2, 3, "abd").compareTo(Generic.of(2, 3, 5));
    }

    /**
     * Test method for {@link Generic#equals(java.lang.Object)}.
     */
    @Test
    public void testEqualsObject() {
        assertTrue(Generic.of(2, 3).equals(Generic.of(2, 3)));
        assertTrue(Generic.of(2, 3, null, "abc").equals(Generic.of(2, 3, null, "abc")));
        assertFalse(Generic.of(2, 3, "abd").equals(Generic.of(2, 3, "abc")));
        assertFalse(Generic.of(2, 3).equals(Generic.of(2, 4)));
        Object t = null;
        assertFalse(Generic.of(2, 3).equals(t));
        assertFalse(Generic.of(2, 3).equals(Generic.of(2, 3, 5)));

        assertTrue(Generic.of().equals(Generic.of()));

        ImmutableGeneric<Integer> gen = Generic.of(2);
        assertTrue(gen.equals(gen));
    }

    /**
     * Test method for {@link Generic#toString()}.
     */
    @Test
    public void testToString() {
        assertEquals("(test, 2)", Generic.of("test", 2).toString());
    }

    /**
     * Test method for {@link Generic#toString(java.lang.String)}.
     */
    @Test
    public void testToStringString() {
        assertEquals("it's a test, value=2", Generic.of("test", 2).toString("it's a %s, value=%s"));
        assertEquals("it's a test, value=2", Generic.of("test", 2).toString("it's a %1$s, value=%2$d"));
        assertEquals("it's a test, null=null, value=2", Generic.of("test", null, 2).toString("it's a %s, null=%s, value=%s"));
    }

    /**
     * Test method for {@link Generic#of(T[])}.
     */
    @Test
    public void testOf() {
        final ImmutableGeneric<Object> generic = Generic.of(2, 3, "test");

        final MutableGeneric<Comparable<?>> genericMutable = Generic.ofMutable(2, 3, "test");

        assertEquals("test", generic.get(2));
        assertEquals(2, genericMutable.set(0, 1));
        assertEquals(1, genericMutable.get(0));

        assertNull(Generic.ofMutable((Object) null).get(0));
    }

    /**
     * Test method for {@link Generic#of(T[])}.
     * 
     * @throws IndexOutOfBoundsException
     *             if out of bounds
     */
    @Test(expected = IndexOutOfBoundsException.class)
    public void testOfSetError() {
        final MutableGeneric<Comparable<?>> genericMutable = Generic.ofMutable(2, 3, "test");

        genericMutable.set(3, 1);
    }

    /**
     * Test method for {@link Generic#of(T[])}.
     */
    @Test(expected = NullPointerException.class)
    public void testOfNull() {
        Generic.ofMutable((Object[]) null);
    }

    /**
     * Test method for {@link Generic#of(T[])}.
     */
    @Test(expected = NullPointerException.class)
    public void testOfAllNull() {
        MutableGeneric<String> mg = Generic.ofMutable("");

        assertEquals(1, mg.getAll().size());

        try {
            Field field = AbstractImmutableGeneric.class.getDeclaredField("all");

            field.setAccessible(true);
            field.set(mg, null);

            mg.getAll();
        } catch (NoSuchFieldException | SecurityException | IllegalArgumentException | IllegalAccessException e) {
            fail();
        }
    }
}
